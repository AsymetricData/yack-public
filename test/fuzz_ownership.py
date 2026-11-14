#!/usr/bin/env python3
"""Differential fuzz of the two ownership checkers against an exact oracle.

Each case is a small Yack function `run (c0, c1, c2)` made of `val`
statements over strings, an Affine record, tokens and borrows: moves,
reads, borrows, field moves, aliases and `if`s.  Every `if` tests its own
condition, and `main` calls `run` with all eight combinations, so the
generator can compute the exact answer: a case is wrong if some
combination moves a moved value, reads one, uses a borrow whose owner was
moved, or does not consume a token exactly once.

Each case is then judged by:

- the frontend borrow checker (NLL): `yack check`;
- the graph (SsaOwn E0900, SsaLoan E0901) alone: `YACK_BORROW_BYPASS=1
  yack check`; an error from the lowering (E04xx) is counted apart;
- AddressSanitizer on the code lowered with neither check
  (`YACK_BORROW_BYPASS=1 YACK_NO_VALIDATE=1`), all eight paths run.

A checker is unsound when it accepts a wrong case, imprecise when it
rejects a right one.  ASan must never report on a right case (that would
be an oracle or lowering bug).

Usage: python3 test/fuzz_ownership.py [count] [seed] [--family NAME]
       (default 400, 1, baseline).  `--family all` runs every family.
`baseline` covers strings, an Affine record, tokens and shared borrows;
`case-datatype` covers `case` on a datatype with an owned payload and on a
recursive list of boxes.
Writes disagreements to $FUZZ_OUT (default /tmp/fuzz_ownership) and exits 1
on any unsound verdict or ASan report on a right case.
"""
import itertools
import os
import random
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor

YACK = os.path.abspath(os.environ.get("YACK", "./yack"))
OUT = os.environ.get("FUZZ_OUT", "/tmp/fuzz_ownership")
CONDITIONS = 3

PRELUDE = """type item : Affine = { name : string, k : int }

fun take (s : string) : int = string_length (borrow s)
fun peek (s : borrow string) : int = string_length (s)
fun take_r (r : item) : int = case r of { name = n, k = k } => take (n) + k
fun peek_r (r : borrow item) : int = case r of { name = n, k = k } => peek (n) + k
fun keep (s : borrow string) : borrow string from s = s
"""

MAIN = """
fun main () =
    let val total =
""" + "\n".join(
    "          {}run ({}, {}, {})".format(
        "+ " if i else "  ", *["true" if b else "false" for b in combo])
    for i, combo in enumerate(itertools.product([True, False], repeat=CONDITIONS))
) + """
    in print_int (total)
    end
"""


class Wrong(Exception):
    pass


FAMILY1_PRELUDE = """fun take (s : string) : int = string_length (borrow s)
fun peek (s : borrow string) : int = string_length (s)

datatype box = Box of string
datatype boxes = NoBoxes | Boxes of box * boxes

fun sum_boxes (values : boxes) : int =
    case values of
        NoBoxes => 0
      | Boxes ((value, rest)) =>
            (case value of Box (text) => take (text)) + sum_boxes (rest)
"""


class Gen:
    """Builds statements and, for each, an effect on an oracle state."""

    def __init__(self, rng):
        self.rng = rng
        self.kinds = {}
        self.counter = 0
        self.conditions = 0
        self.lines = []
        self.effects = []
        self.ints = []

    def fresh(self, prefix):
        self.counter += 1
        return "{}{}".format(prefix, self.counter)

    def pick(self, kind, alive):
        """A variable of `kind`, preferably one still usable on every path."""
        names = [n for n, k in self.kinds.items() if k == kind]
        if not names:
            return None
        good = [n for n in names if n in alive]
        if good and self.rng.random() < 0.95:
            return self.rng.choice(good)
        return self.rng.choice(names)


    @staticmethod
    def need(state, name):
        if state[name]["moved"]:
            raise Wrong("use of moved " + name)

    @staticmethod
    def move(state, name):
        Gen.need(state, name)
        if state[name].get("partial"):
            raise Wrong("move of partially moved " + name)
        state[name]["moved"] = True


    def int_op(self, alive):
        """Returns (source, effect) or None."""
        choices = []
        s = self.pick("str", alive)
        r = self.pick("item", alive)
        b = self.pick("bor", alive)
        if s:
            choices += [
                ("take ({})".format(s), lambda st, s=s: Gen.move(st, s)),
                ("peek (borrow {})".format(s), lambda st, s=s: Gen.need(st, s)),
            ]
        if r:
            def whole_read(st, r=r):
                Gen.need(st, r)
                if st[r].get("partial"):
                    raise Wrong("read of partially moved " + r)

            def field_move(st, r=r):
                Gen.need(st, r)
                if st[r].get("partial"):
                    raise Wrong("field moved twice " + r)
                st[r]["partial"] = True

            choices += [
                ("take_r ({})".format(r), lambda st, r=r: Gen.move(st, r)),
                ("peek_r (borrow {})".format(r), whole_read),
                ("take ({}.name)".format(r), field_move),
                ("{}.k".format(r), lambda st, r=r: Gen.need(st, r)),
            ]
        if b:
            def use_borrow(st, b=b):
                owner = st[b]["owner"]
                if st[owner]["moved"]:
                    raise Wrong("borrow {} used after {} moved".format(b, owner))
            choices.append(("peek ({})".format(b), use_borrow))
        if not choices:
            return None
        return self.rng.choice(choices)

    def statement(self):
        alive = self.alive_everywhere()
        roll = self.rng.random()
        if roll < 0.12:
            name = self.fresh("s")
            self.kinds[name] = "str"
            self.emit("val {} = \"{}\" ^ \"!\"".format(name, name),
                      lambda st, c, n=name: st.__setitem__(n, {"moved": False}))
        elif roll < 0.20:
            s = self.pick("str", alive)
            if not s:
                return
            name = self.fresh("s")
            self.kinds[name] = "str"

            def alias(st, c, s=s, n=name):
                Gen.move(st, s)
                st[n] = {"moved": False}
            self.emit("val {} = {}".format(name, s), alias)
        elif roll < 0.27:
            s = self.pick("str", alive)
            if not s:
                return
            name = self.fresh("r")
            self.kinds[name] = "item"

            def build(st, c, s=s, n=name):
                Gen.move(st, s)
                st[n] = {"moved": False}
            self.emit("val {} : item = {{ name = {}, k = 2 }}".format(name, s), build)
        elif roll < 0.35:
            s = self.pick("str", alive)
            if not s:
                return
            name = self.fresh("b")
            self.kinds[name] = "bor"
            via = self.rng.choice(["borrow {}", "keep (borrow {})"]).format(s)

            def lend(st, c, s=s, n=name):
                Gen.need(st, s)
                st[n] = {"moved": False, "owner": s}
            self.emit("val {} = {}".format(name, via), lend)
        elif roll < 0.41:
            name = self.fresh("t")
            self.kinds[name] = "tok"
            self.emit("val {} = new_token ()".format(name),
                      lambda st, c, n=name: st.__setitem__(n, {"moved": False}))
        elif roll < 0.49:
            t = self.pick("tok", alive)
            if not t:
                return
            if self.conditions < CONDITIONS and self.rng.random() < 0.3:
                k = self.conditions
                self.conditions += 1

                def maybe(st, c, t=t, k=k):
                    if c[k]:
                        Gen.move(st, t)
                self.emit("val _ = if c{} then consume_token ({}) else ()".format(k, t), maybe)
            else:
                self.emit("val _ = consume_token ({})".format(t),
                          lambda st, c, t=t: Gen.move(st, t))
        else:
            first = self.int_op(alive)
            if not first:
                return
            name = self.fresh("x")
            self.ints.append(name)
            if self.conditions < CONDITIONS and self.rng.random() < 0.35:
                second = self.int_op(alive) or ("0", lambda st: None)
                k = self.conditions
                self.conditions += 1

                def branch(st, c, a=first[1], b=second[1], k=k):
                    (a if c[k] else b)(st)
                self.emit("val {} = if c{} then {} else {}".format(
                    name, k, first[0], second[0]), branch)
            else:
                self.emit("val {} = {}".format(name, first[0]),
                          lambda st, c, f=first[1]: f(st))

    def emit(self, line, effect):
        self.lines.append(line)
        self.effects.append(effect)

    def run_path(self, combo):
        state = {}
        for effect in self.effects:
            effect(state, combo)
        for name, kind in self.kinds.items():
            if kind == "tok" and name in state and not state[name]["moved"]:
                raise Wrong("token {} not consumed".format(name))

    def alive_everywhere(self):
        """Names usable on every path after the statements so far."""
        alive = None
        for combo in itertools.product([True, False], repeat=CONDITIONS):
            state = {}
            try:
                for effect in self.effects:
                    effect(state, combo)
            except Wrong:
                return set()
            here = {n for n, v in state.items()
                    if not v["moved"] and not v.get("partial")
                    and not (self.kinds[n] == "bor" and state[v["owner"]]["moved"])}
            alive = here if alive is None else alive & here
        return alive or set()

    def verdict(self):
        reasons = set()
        for combo in itertools.product([True, False], repeat=CONDITIONS):
            try:
                self.run_path(combo)
            except Wrong as error:
                reasons.add(str(error))
        return sorted(reasons)

    def source(self):
        body = "\n".join("        " + line for line in self.lines)
        result = " + ".join(self.ints) if self.ints else "0"
        return (PRELUDE +
                "\nfun run (c0 : bool, c1 : bool, c2 : bool) : int =\n"
                "    let\n" + body + "\n    in " + result + "\n    end\n" + MAIN)


class CaseGen(Gen):
    """Family: `case` on datatypes with an owned payload, and a recursive
    list of boxes.  Matching consumes the scrutinee and moves the payload
    into the pattern bindings."""

    def int_op(self, alive):
        choices = []
        s = self.pick("str", alive)
        b = self.pick("box", alive)
        l = self.pick("boxes", alive)
        if s:
            choices += [
                ("take ({})".format(s), lambda st, s=s: Gen.move(st, s)),
                ("peek (borrow {})".format(s), lambda st, s=s: Gen.need(st, s)),
            ]
        if b:
            def consume(st, b=b):
                Gen.need(st, b)
                Gen.move(st, b)
            choices += [
                ("case {} of Box (t) => take (t)".format(b),
                 lambda st, b=b: consume(st, b)),
                ("case {} of Box (t) => peek (borrow t)".format(b),
                 lambda st, b=b: consume(st, b)),
            ]
        if l:
            def drain(st, l=l):
                Gen.need(st, l)
                Gen.move(st, l)
            choices.append(("sum_boxes ({})".format(l),
                            lambda st, l=l: drain(st, l)))
        if not choices:
            return None
        return self.rng.choice(choices)

    def statement(self):
        alive = self.alive_everywhere()
        roll = self.rng.random()
        if roll < 0.16:
            name = self.fresh("s")
            self.kinds[name] = "str"
            self.emit('val {} = "{}" ^ "!"'.format(name, name),
                      lambda st, c, n=name: st.__setitem__(n, {"moved": False}))
        elif roll < 0.32:
            s = self.pick("str", alive)
            if not s:
                return
            name = self.fresh("b")
            self.kinds[name] = "box"

            def build(st, c, s=s, n=name):
                Gen.move(st, s)
                st[n] = {"moved": False}
            self.emit("val {} = Box ({})".format(name, s), build)
        elif roll < 0.48:
            b = self.pick("box", alive)
            l = self.pick("boxes", alive)
            if not b or not l:
                return
            name = self.fresh("l")
            self.kinds[name] = "boxes"

            def cons(st, c, b=b, l=l, n=name):
                Gen.move(st, b)
                Gen.move(st, l)
                st[n] = {"moved": False}
            self.emit("val {} = Boxes (({}, {}))".format(name, b, l), cons)
        elif roll < 0.70 and self.conditions < CONDITIONS:
            first = self.int_op(alive)
            if not first:
                return
            name = self.fresh("x")
            self.ints.append(name)
            second = self.int_op(alive) or ("0", lambda st: None)
            k = self.conditions
            self.conditions += 1

            def branch(st, c, a=first[1], b=second[1], k=k):
                (a if c[k] else b)(st)
            self.emit("val {} = if c{} then {} else {}".format(
                name, k, first[0], second[0]), branch)
        else:
            first = self.int_op(alive)
            if not first:
                return
            name = self.fresh("x")
            self.ints.append(name)
            self.emit("val {} = {}".format(name, first[0]),
                      lambda st, c, f=first[1]: f(st))

    def source(self):
        body = "\n".join("        " + line for line in self.lines)
        result = " + ".join(self.ints) if self.ints else "0"
        return (FAMILY1_PRELUDE +
                "\nfun run (c0 : bool, c1 : bool, c2 : bool) : int =\n"
                "    let\n" + body + "\n    in " + result + "\n    end\n" + MAIN)


def generate_case(seed):
    rng = random.Random(seed)
    gen = CaseGen(rng)
    for _ in range(2):
        name = gen.fresh("s")
        gen.kinds[name] = "str"
        gen.emit('val {} = "{}" ^ "!"'.format(name, name),
                 lambda st, c, n=name: st.__setitem__(n, {"moved": False}))
    payload = gen.fresh("s")
    gen.kinds[payload] = "str"
    gen.emit('val {} = "{}" ^ "#"'.format(payload, payload),
             lambda st, c, n=payload: st.__setitem__(n, {"moved": False}))
    box = gen.fresh("b")
    gen.kinds[box] = "box"

    def make(st, c, s=payload, n=box):
        Gen.move(st, s)
        st[n] = {"moved": False}
    gen.emit("val {} = Box ({})".format(box, payload), make)
    empty = gen.fresh("l")
    gen.kinds[empty] = "boxes"
    gen.emit("val {} = NoBoxes".format(empty),
             lambda st, c, n=empty: st.__setitem__(n, {"moved": False}))
    for _ in range(rng.randint(3, 12)):
        gen.statement()
    return gen


FAMILY2_PRELUDE = """type cell : Affine = { value : int, name : string }
type point : Copy = { x : int, y : int }

fun touch_cell (c : borrow mut cell) : unit = ()
fun read_cell (c : borrow cell) : int = c.value
fun touch_text (x : borrow mut string) : unit = ()
fun read_text (x : borrow string) : int = string_length (x)
fun touch_int (x : borrow mut int) : unit = ()
fun read_int (x : borrow int) : int = 0
"""


class BorrowGen:
    """Family: mutable and shared borrows of an Affine record and its
    fields.  A loan lives from its creation until its last use; while it is
    live, the place cannot be moved, and a mutable loan also forbids reads
    and new borrows."""

    def __init__(self, rng):
        self.rng = rng
        self.counter = 0
        self.lines = []
        self.effects = []
        self.ints = []
        self.cell = None
        self.point = None
        self.mut = []
        self.shared = []

    def fresh(self, prefix):
        self.counter += 1
        return "{}{}".format(prefix, self.counter)

    def emit(self, line, effect):
        self.lines.append(line)
        self.effects.append(effect)

    def loan(self, mutable, field):
        name = self.fresh("m" if mutable else "b")
        if field is None:
            text = "borrow mut {}".format(self.cell) if mutable \
                else "borrow {}".format(self.cell)
        else:
            text = ("borrow mut {}.{}" if mutable else "borrow {}.{}").format(
                self.cell, field)
        self.emit("val {} = {}".format(name, text),
                  ("make_loan", name, self.cell, field, mutable))
        (self.mut if mutable else self.shared).append((name, field))

    def use_loan(self, mutable):
        pool = self.mut if mutable else self.shared
        if not pool:
            return False
        name, field = self.rng.choice(pool)
        if mutable:
            call = ("touch_cell" if field is None else
                    "touch_text" if field == "name" else "touch_int")
            self.emit("val _ = {} ({})".format(call, name),
                      ("use_mut", name))
        else:
            call = ("read_cell" if field is None else
                    "read_text" if field == "name" else "read_int")
            target = self.fresh("x")
            self.ints.append(target)
            self.emit("val {} = {} ({})".format(target, call, name),
                      ("use_shared", name))
        return True

    def statement(self):
        roll = self.rng.random()
        c = self.cell
        if roll < 0.16:
            self.loan(True, None)
        elif roll < 0.30:
            self.loan(False, None)
        elif roll < 0.42:
            self.loan(True, "name")
        elif roll < 0.50:
            self.loan(True, "value")
        elif roll < 0.58:
            self.loan(False, "name")
        elif roll < 0.68:
            target = self.fresh("x")
            self.ints.append(target)
            self.emit("val {} = {}.value".format(target, c),
                      ("read_field", c, "value"))
        elif roll < 0.76:
            self.emit("val {} = {}".format(self.fresh("d"), c),
                      ("move_owner", c))
        elif roll < 0.84:
            self.emit("val {} = {}.name".format(self.fresh("n"), c),
                      ("move_field", c, "name"))
        elif roll < 0.92:
            if not self.use_loan(True):
                self.loan(True, None)
        elif roll < 0.97:
            if not self.use_loan(False):
                self.loan(False, None)
        else:
            target = self.fresh("x")
            self.ints.append(target)
            self.emit("val {} = {}.x".format(target, self.point),
                      ("read_copy", self.point))

    def source(self):
        body = "\n".join("        " + line for line in self.lines)
        result = " + ".join(self.ints) if self.ints else "0"
        return (FAMILY2_PRELUDE +
                "\nfun run (c0 : bool, c1 : bool, c2 : bool) : int =\n"
                "    let\n" + body + "\n    in " + result + "\n    end\n" + MAIN)

    def verdict(self):
        last = {}
        for index, effect in enumerate(self.effects):
            if effect[0] in ("use_shared", "use_mut"):
                last[effect[1]] = index
        values = {}
        loans = []

        def need(name):
            value = values.get(name)
            if value is None or value["moved"]:
                raise Wrong("use of moved " + name)
            return value

        def overlap(loan, var, field):
            if loan["var"] != var:
                return False
            return loan["place"] is None or field is None or \
                loan["place"] == field

        def active(index):
            return [loan for loan in loans
                    if loan["created"] < index and
                    last.get(loan["name"], -1) >= index]

        try:
            for index, effect in enumerate(self.effects):
                kind = effect[0]
                if kind == "new_str":
                    values[effect[1]] = {"kind": "str", "moved": False,
                                         "partial": set()}
                elif kind == "new_cell":
                    need(effect[2])
                    values[effect[2]]["moved"] = True
                    values[effect[1]] = {"kind": "cell", "moved": False,
                                         "partial": set()}
                elif kind == "new_point":
                    values[effect[1]] = {"kind": "point", "moved": False,
                                         "partial": set()}
                elif kind == "read_field":
                    _, var, field = effect
                    value = need(var)
                    if field in value["partial"]:
                        raise Wrong("read of moved field")
                    for loan in active(index):
                        if loan["mutable"] and overlap(loan, var, field):
                            raise Wrong("read under mutable borrow")
                elif kind == "read_copy":
                    need(effect[1])
                elif kind == "move_field":
                    _, var, field = effect
                    value = need(var)
                    if field in value["partial"]:
                        raise Wrong("field moved twice")
                    if any(overlap(loan, var, field) for loan in active(index)):
                        raise Wrong("move while borrowed")
                    value["partial"].add(field)
                elif kind == "move_owner":
                    var = effect[1]
                    value = need(var)
                    if value["partial"]:
                        raise Wrong("move of partially moved")
                    if any(overlap(loan, var, None) for loan in active(index)):
                        raise Wrong("move while borrowed")
                    value["moved"] = True
                elif kind == "make_loan":
                    _, name, var, field, mutable = effect
                    value = need(var)
                    if field is not None and field in value["partial"]:
                        raise Wrong("borrow of moved field")
                    if field is None and value["partial"]:
                        raise Wrong("borrow of partially moved")
                    for loan in active(index):
                        if not overlap(loan, var, field):
                            continue
                        if mutable:
                            raise Wrong("mutable borrow overlaps a borrow")
                        if loan["mutable"]:
                            raise Wrong("shared borrow under a mutable borrow")
                    loans.append({"name": name, "var": var, "place": field,
                                  "mutable": mutable, "created": index,
                                  "used": False})
                elif kind == "use_shared":
                    loan = [l for l in loans if l["name"] == effect[1]][0]
                    value = need(loan["var"])
                    if loan["place"] is not None and \
                       loan["place"] in value["partial"]:
                        raise Wrong("use of moved field")
                elif kind == "use_mut":
                    loan = [l for l in loans if l["name"] == effect[1]][0]
                    if loan["used"]:
                        raise Wrong("mutable borrow used twice")
                    value = need(loan["var"])
                    if loan["place"] is not None and \
                       loan["place"] in value["partial"]:
                        raise Wrong("use of moved field")
                    loan["used"] = True
        except Wrong as error:
            return [str(error)]
        return []


def generate_borrow(seed):
    rng = random.Random(seed)
    gen = BorrowGen(rng)
    text = gen.fresh("s")
    gen.emit('val {} = "{}" ^ "#"'.format(text, text), ("new_str", text))
    cell = gen.fresh("c")
    gen.cell = cell
    gen.emit("val {} : cell = {{ value = 1, name = {} }}".format(cell, text),
             ("new_cell", cell, text))
    point = gen.fresh("p")
    gen.point = point
    gen.emit("val {} : point = {{ x = 1, y = 2 }}".format(point),
             ("new_point", point))
    if rng.random() < 0.5:
        gen.loan(True, None)
    else:
        gen.loan(False, None)
    for _ in range(rng.randint(3, 9)):
        gen.statement()
    return gen


class ContainerGen:
    """Family: element borrows of a map and an array, and mutations of the
    container while a borrow of one of its elements is live.  The borrow is
    a shared loan on the container: reads are fine, a mutation or a move is
    not."""

    def __init__(self, rng):
        self.rng = rng
        self.counter = 0
        self.lines = []
        self.effects = []
        self.ints = []
        self.map = None
        self.array = None
        self.loans = []

    def fresh(self, prefix):
        self.counter += 1
        return "{}{}".format(prefix, self.counter)

    def emit(self, line, effect):
        self.lines.append(line)
        self.effects.append(effect)

    def get_element(self):
        container = self.rng.choice([self.map, self.array])
        name = self.fresh("b")
        if container == self.map:
            text = 'map_get_borrow ({}, "k")'.format(container)
        else:
            text = "array_get_borrow ({}, 0)".format(container)
        self.emit("val {} = {}".format(name, text),
                  ("get_elem", name, container))
        self.loans.append((name, container))

    def use_element(self):
        if not self.loans:
            return False
        name, _ = self.rng.choice(self.loans)
        target = self.fresh("x")
        self.ints.append(target)
        self.emit("val {} = string_length ({})".format(target, name),
                  ("use_elem", name))
        return True

    def mutate(self, container):
        if container == self.map:
            roll = self.rng.random()
            if roll < 0.6:
                line = 'val _ = map_set ({}, "x", "c" ^ "d")'.format(container)
            else:
                line = 'val _ = map_remove ({}, "j")'.format(container)
        else:
            roll = self.rng.random()
            if roll < 0.6:
                line = 'val _ = array_set ({}, 0, "c" ^ "d")'.format(container)
            else:
                line = 'val _ = array_push ({}, "e" ^ "f")'.format(container)
        self.emit(line, ("mutate", container))

    def statement(self):
        roll = self.rng.random()
        if roll < 0.28:
            self.get_element()
        elif roll < 0.48:
            if not self.use_element():
                self.get_element()
        elif roll < 0.62:
            self.mutate(self.map)
        elif roll < 0.74:
            self.mutate(self.array)
        elif roll < 0.84:
            container = self.rng.choice([self.map, self.array])
            target = self.fresh("x")
            self.ints.append(target)
            call = "map_length" if container == self.map else "array_length"
            self.emit("val {} = {} ({})".format(target, call, container),
                      ("read_len", container))
        else:
            container = self.rng.choice([self.map, self.array])
            self.emit("val {} = {}".format(self.fresh("d"), container),
                      ("move", container))

    def source(self):
        body = "\n".join("        " + line for line in self.lines)
        result = " + ".join(self.ints) if self.ints else "0"
        return ("fun run (c0 : bool, c1 : bool, c2 : bool) : int =\n"
                "    let\n" + body + "\n    in " + result + "\n    end\n" + MAIN)

    def verdict(self):
        last = {}
        for index, effect in enumerate(self.effects):
            if effect[0] == "use_elem":
                last[effect[1]] = index
        values = {}
        loans = []

        def need(name):
            value = values.get(name)
            if value is None or value["moved"]:
                raise Wrong("use of moved " + name)
            return value

        def active(index, container):
            return [loan for loan in loans
                    if loan["container"] == container and
                    loan["created"] < index and
                    last.get(loan["name"], -1) >= index]

        try:
            for index, effect in enumerate(self.effects):
                kind = effect[0]
                if kind == "new_str":
                    values[effect[1]] = {"moved": False}
                elif kind == "new_map":
                    values[effect[1]] = {"moved": False}
                elif kind == "new_array":
                    values[effect[1]] = {"moved": False}
                elif kind == "map_put":
                    need(effect[1])
                    need(effect[2])
                    values[effect[2]]["moved"] = True
                elif kind == "array_push_lit":
                    need(effect[1])
                elif kind == "get_elem":
                    _, name, container = effect
                    need(container)
                    loans.append({"name": name, "container": container,
                                  "created": index})
                elif kind == "use_elem":
                    loan = [l for l in loans if l["name"] == effect[1]][0]
                    need(loan["container"])
                elif kind == "mutate":
                    container = effect[1]
                    need(container)
                    if active(index, container):
                        raise Wrong("mutate while an element is borrowed")
                elif kind == "read_len":
                    need(effect[1])
                elif kind == "move":
                    container = effect[1]
                    need(container)
                    if active(index, container):
                        raise Wrong("move while an element is borrowed")
                    values[container]["moved"] = True
        except Wrong as error:
            return [str(error)]
        return []


def generate_container(seed):
    rng = random.Random(seed)
    gen = ContainerGen(rng)
    first = gen.fresh("s")
    gen.emit('val {} = "{}" ^ "#"'.format(first, first), ("new_str", first))
    second = gen.fresh("s")
    gen.emit('val {} = "{}" ^ "?"'.format(second, second), ("new_str", second))
    mapping = gen.fresh("m")
    gen.map = mapping
    gen.emit("val {} : string map = map_new ()".format(mapping),
             ("new_map", mapping))
    gen.emit('val _ = map_set ({}, "k", {})'.format(mapping, first),
             ("map_put", mapping, first))
    gen.emit('val _ = map_set ({}, "j", {})'.format(mapping, second),
             ("map_put", mapping, second))
    array = gen.fresh("a")
    gen.array = array
    gen.emit("val {} : string array = array_new ()".format(array),
             ("new_array", array))
    gen.emit('val _ = array_push ({}, "a0" ^ "!")'.format(array),
             ("array_push_lit", array))
    for _ in range(rng.randint(4, 9)):
        gen.statement()
    return gen


FAMILY6_PRELUDE = """type leaf = { name : string, k : int }
type inner = { leaf : leaf, label : string }
type outer = { inner : inner, tag : string }
"""


class NestedGen:
    """Family: partial moves on two levels of records.  A path is moved
    once; an ancestor or a descendant that was moved forbids moving it
    again, and a moved ancestor forbids reading its Copy fields."""

    MOVES = [
        (("inner", "leaf", "name"), "o.inner.leaf.name"),
        (("inner", "label"), "o.inner.label"),
        (("tag",), "o.tag"),
        (("inner", "leaf"), "o.inner.leaf"),
        (("inner",), "o.inner"),
        ((), "o"),
    ]
    READ = (("inner", "leaf", "k"), "o.inner.leaf.k")

    def __init__(self, rng):
        self.rng = rng
        self.counter = 0
        self.lines = []
        self.effects = []
        self.ints = []
        self.planned = []

    def fresh(self, prefix):
        self.counter += 1
        return "{}{}".format(prefix, self.counter)

    def emit(self, line, effect):
        self.lines.append(line)
        self.effects.append(effect)

    def compatible(self, path):
        def prefix(short, long):
            return len(short) <= len(long) and long[:len(short)] == short
        return not any(prefix(path, other) or prefix(other, path)
                       for other in self.planned)

    def statement(self):
        available = [entry for entry in self.MOVES
                     if self.compatible(entry[0])]
        if not available or self.rng.random() < 0.22:
            target = self.fresh("x")
            self.ints.append(target)
            self.emit("val {} = {}".format(target, self.READ[1]),
                      ("read_copy", self.READ[0]))
        else:
            path, text = self.rng.choice(available)
            self.planned.append(path)
            self.emit("val {} = {}".format(self.fresh("v"), text),
                      ("move", path))

    def source(self):
        body = "\n".join("        " + line for line in self.lines)
        result = " + ".join(self.ints) if self.ints else "0"
        return (FAMILY6_PRELUDE +
                "\nfun run (c0 : bool, c1 : bool, c2 : bool) : int =\n"
                "    let val o : outer =\n"
                "            { inner = { leaf = { name = \"a\" ^ \"b\", k = 1 }"
                ", label = \"c\" ^ \"d\" }\n"
                "            , tag = \"e\" ^ \"f\"\n"
                "            }\n" + body + "\n    in " + result +
                "\n    end\n" + MAIN)

    def verdict(self):
        def prefix(short, long):
            return len(short) <= len(long) and long[:len(short)] == short

        moved = []
        try:
            for effect in self.effects:
                if effect[0] == "move":
                    path = effect[1]
                    for other in moved:
                        if prefix(path, other) or prefix(other, path):
                            raise Wrong("move of a moved or partial path")
                    moved.append(path)
                elif effect[0] == "read_copy":
                    path = effect[1]
                    for other in moved:
                        if prefix(other, path):
                            raise Wrong("read of a field under a moved path")
        except Wrong as error:
            return [str(error)]
        return []


def generate_nested(seed):
    rng = random.Random(seed)
    gen = NestedGen(rng)
    for _ in range(rng.randint(3, 8)):
        gen.statement()
    return gen


FAMILY5_PRELUDE = """fun pass (x : 'a) : 'a = x
fun first (pair : 'a * 'b) : 'a = case pair of (a, b) => a
fun wrap (x : 'a) : 'a list = Cons ((x, Nil))

fun sum_lengths (values : string list) : int =
    case values of
        Nil => 0
      | Cons ((head, rest)) => string_length (borrow head) + sum_lengths (rest)

fun sum_ints (values : int list) : int =
    case values of
        Nil => 0
      | Cons ((head, rest)) => head + sum_ints (rest)
"""


class GenericGen:
    """Family: generic functions instantiated with Affine and Copy types.
    `pass`, `first` and `wrap` consume an Affine argument and copy a Copy
    one."""

    OWNS = {"str", "pair_str", "pair_int", "list_str", "list_int"}

    def __init__(self, rng):
        self.rng = rng
        self.counter = 0
        self.lines = []
        self.effects = []
        self.ints = []
        self.kinds = {}
        self.components = {}
        self.planned = set()

    def fresh(self, prefix):
        self.counter += 1
        return "{}{}".format(prefix, self.counter)

    def emit(self, line, effect):
        self.lines.append(line)
        self.effects.append(effect)

    def alive(self, kinds):
        alive = set()
        for name, kind in self.kinds.items():
            if kind in kinds:
                alive.add(name)
        return sorted(alive)

    def pick(self, pool):
        good = [name for name in pool if name not in self.planned]
        if good and self.rng.random() < 0.9:
            return self.rng.choice(good)
        return self.rng.choice(pool)

    def spent(self, name):
        kind = self.kinds[name]
        self.planned.add(name)
        if kind == "pair_str" and name in self.components:
            self.planned.add(self.components[name])

    def statement(self):
        roll = self.rng.random()
        strings = self.alive({"str"})
        ints = self.alive({"int"})
        pairs_str = self.alive({"pair_str"})
        pairs_int = self.alive({"pair_int"})
        lists = self.alive({"list_str"})
        any_owned = strings + pairs_str + lists
        if roll < 0.30 and any_owned:
            source = self.pick(any_owned)
            kind = self.kinds[source]
            if kind in self.OWNS:
                self.spent(source)
            target = self.fresh("x")
            self.kinds[target] = kind
            self.emit("val {} = pass ({})".format(target, source),
                      ("op", source, kind, target, kind))
        elif roll < 0.48 and (strings or ints):
            source = self.pick(strings + ints)
            kind = self.kinds[source]
            if kind in self.OWNS:
                self.spent(source)
            target = self.fresh("l")
            self.kinds[target] = ("list_str" if kind == "str"
                                  else "list_int")
            self.emit("val {} = wrap ({})".format(target, source),
                      ("op", source, kind, target, self.kinds[target]))
        elif roll < 0.62 and (pairs_str or pairs_int):
            source = self.pick(pairs_str + pairs_int)
            source_kind = self.kinds[source]
            if source_kind in self.OWNS:
                self.spent(source)
            kind = "str" if source_kind == "pair_str" else "int"
            target = self.fresh("x" if kind == "str" else "m")
            self.kinds[target] = kind
            if kind == "int":
                self.ints.append(target)
            self.emit("val {} = first ({})".format(target, source),
                      ("op", source, source_kind, target, kind))
        elif roll < 0.76 and strings:
            source = self.pick(strings)
            target = self.fresh("x")
            self.ints.append(target)
            self.emit("val {} = string_length (borrow {})".format(target, source),
                      ("use_str", source))
        elif roll < 0.88 and (lists or self.alive({"list_int"})):
            source = self.pick(lists + self.alive({"list_int"}))
            self.spent(source)
            target = self.fresh("m")
            self.ints.append(target)
            helper = ("sum_lengths" if self.kinds[source] == "list_str"
                      else "sum_ints")
            self.emit("val {} = {} ({})".format(target, helper, source),
                      ("use_list", source))
        elif strings or pairs_str or lists:

            pool = strings + pairs_str + lists
            source = self.pick(pool)
            kind = self.kinds[source]
            self.spent(source)
            target = self.fresh("x")
            self.kinds[target] = kind
            self.emit("val {} = pass ({})".format(target, source),
                      ("op", source, kind, target, kind))

    def source(self):
        body = "\n".join("        " + line for line in self.lines)
        result = " + ".join(self.ints) if self.ints else "0"
        return (FAMILY5_PRELUDE +
                "\nfun run (c0 : bool, c1 : bool, c2 : bool) : int =\n"
                "    let\n" + body + "\n    in " + result + "\n    end\n" + MAIN)

    def verdict(self):
        moved = set()
        try:
            for effect in self.effects:
                kind = effect[0]
                if kind == "new_pair":
                    _, pair, component = effect
                    if component in moved:
                        raise Wrong("build a pair from a moved string")
                    moved.add(component)
                elif kind == "op":
                    _, source, source_kind, target, target_kind = effect
                    if source in moved:
                        raise Wrong("pass a moved value to a generic")
                    if source_kind in self.OWNS:
                        moved.add(source)
                    if source_kind == "pair_str":
                        component = self.components.get(source)
                        if component is not None:
                            moved.add(component)
                elif kind == "use_str":
                    if effect[1] in moved:
                        raise Wrong("read of a moved value")
                elif kind == "use_list":
                    if effect[1] in moved:
                        raise Wrong("consume of a moved list")
                    moved.add(effect[1])
        except Wrong as error:
            return [str(error)]
        return []


def generate_generic(seed):
    rng = random.Random(seed)
    gen = GenericGen(rng)
    first = gen.fresh("s")
    gen.kinds[first] = "str"
    gen.emit('val {} = "{}" ^ "#"'.format(first, first), ("new", first))
    second = gen.fresh("s")
    gen.kinds[second] = "str"
    gen.emit('val {} = "{}" ^ "?"'.format(second, second), ("new", second))
    number = gen.fresh("n")
    gen.kinds[number] = "int"
    gen.emit("val {} = 7".format(number), ("new", number))
    pair_str = gen.fresh("p")
    gen.kinds[pair_str] = "pair_str"
    gen.components[pair_str] = first
    gen.planned.add(first)
    gen.emit("val {} = ({}, 3)".format(pair_str, first), ("new_pair", pair_str, first))
    pair_int = gen.fresh("p")
    gen.kinds[pair_int] = "pair_int"
    gen.emit("val {} = (4, 5)".format(pair_int), ("new", pair_int))
    for _ in range(rng.randint(4, 9)):
        gen.statement()
    return gen


class GuardGen:
    """Family: `guard Ok value <- result else ...`.  The guard consumes the
    result and binds the owned payload; both are then tracked like any
    other value."""

    def __init__(self, rng):
        self.rng = rng
        self.counter = 0
        self.lines = []
        self.effects = []
        self.ints = []
        self.texts = []
        self.results = []
        self.sources = []
        self.moved = set()

    def fresh(self, prefix):
        self.counter += 1
        return "{}{}".format(prefix, self.counter)

    def emit(self, line, effect):
        self.lines.append(line)
        self.effects.append(effect)

    def new_result(self):
        source = self.fresh("s")
        self.emit('val {} = "{}" ^ "#"'.format(source, source),
                  ("new_str", source))
        wrapped = self.fresh("r")
        self.emit("val {} : string result = Ok ({})".format(wrapped, source),
                  ("wrap_ok", wrapped, source))
        text = self.fresh("t")
        self.emit("guard Ok {} <- {} else 0".format(text, wrapped),
                  ("guard_ok", wrapped, text))
        self.sources.append(source)
        self.results.append(wrapped)
        self.texts.append(text)

    def alive_texts(self):
        return [name for name in self.texts if name not in self.moved]

    def pick_text(self):
        good = self.alive_texts()
        if good and self.rng.random() < 0.9:
            return self.rng.choice(good)
        return self.rng.choice(self.texts)

    def use_string(self, target, name):
        self.ints.append(target)
        self.emit("val {} = string_length (borrow {})".format(target, name),
                  ("use_str", name))

    def statement(self):
        roll = self.rng.random()
        if roll < 0.30:
            self.use_string(self.fresh("x"), self.pick_text())
        elif roll < 0.44:
            name = self.pick_text()
            self.emit("val {} = {}".format(self.fresh("v"), name),
                      ("move_str", name))
        elif roll < 0.58:
            self.new_result()
        elif roll < 0.68 and self.results:
            name = self.rng.choice(self.results)
            target = self.fresh("x")
            self.ints.append(target)
            self.emit("val {} = result_len ({})".format(target, name),
                      ("use_str", name))
        elif roll < 0.78 and self.sources:
            self.use_string(self.fresh("x"), self.rng.choice(self.sources))
        elif roll < 0.92:
            condition = self.rng.choice(["c0", "c1", "c2"])
            self.emit("guard {} else 0".format(condition), ("guard_cond",))
        else:
            self.use_string(self.fresh("x"), self.pick_text())

    def source(self):
        body = "\n".join("        " + line for line in self.lines)
        result = " + ".join(self.ints) if self.ints else "0"
        return ("fun result_len (value : string result) : int = 0\n"
                "\nfun run (c0 : bool, c1 : bool, c2 : bool) : int =\n"
                "    let\n" + body + "\n    in " + result + "\n    end\n" + MAIN)

    def verdict(self):
        values = set()
        moved = set()

        def need(name):
            if name not in values or name in moved:
                raise Wrong("use of moved " + name)

        try:
            for effect in self.effects:
                kind = effect[0]
                if kind == "new_str":
                    values.add(effect[1])
                elif kind == "wrap_ok":
                    _, wrapped, source = effect
                    need(source)
                    moved.add(source)
                    values.add(wrapped)
                elif kind == "guard_ok":
                    _, wrapped, text = effect
                    need(wrapped)
                    moved.add(wrapped)
                    values.add(text)
                elif kind == "use_str":
                    need(effect[1])
                elif kind == "move_str":
                    need(effect[1])
                    moved.add(effect[1])
                elif kind == "guard_cond":
                    pass
        except Wrong as error:
            return [str(error)]
        return []


def generate_guard(seed):
    rng = random.Random(seed)
    gen = GuardGen(rng)
    gen.new_result()
    for _ in range(rng.randint(3, 8)):
        gen.statement()
    return gen


FAMILY4_PRELUDE = """fun sink (text : string) : unit = ()

fun apply (f : consume string -> unit, text : string) : unit = f (text)
"""


class CallbackGen:
    """Family: callbacks.  A named function owns its parameter and may be
    passed where the callback consumes; an inline lambda takes its ownership
    from the expected callback type.  A lambda bound by `val` only borrows
    its parameter, so passing it to a consuming callback or calling it on an
    owned argument is rejected (E0514/E0517)."""

    def __init__(self, rng):
        self.rng = rng
        self.counter = 0
        self.lines = []
        self.effects = []
        self.ints = []
        self.strings = []
        self.lambdas = []
        self.moved = set()

    def fresh(self, prefix):
        self.counter += 1
        return "{}{}".format(prefix, self.counter)

    def emit(self, line, effect):
        self.lines.append(line)
        self.effects.append(effect)

    def pick_string(self):
        good = [name for name in self.strings if name not in self.moved]
        if good and self.rng.random() < 0.9:
            return self.rng.choice(good)
        return self.rng.choice(self.strings)

    def statement(self):
        roll = self.rng.random()
        if roll < 0.20:
            name = self.fresh("s")
            self.strings.append(name)
            self.emit('val {} = "{}" ^ "!"'.format(name, name),
                      ("new_str", name))
        elif roll < 0.35:
            name = self.fresh("f")
            self.lambdas.append(name)
            self.emit("val {} = x => print_string (x)".format(name),
                      ("new_fun", name))
        elif roll < 0.55:
            target = self.pick_string()
            self.emit("val _ = apply (sink, {})".format(target),
                      ("apply_str", target))
        elif roll < 0.75:
            target = self.pick_string()
            self.emit("val _ = apply (x => print_string (x), {})".format(
                target), ("apply_str", target))
        elif roll < 0.82 and self.lambdas:
            target = self.pick_string()
            name = self.rng.choice(self.lambdas)
            self.emit("val _ = {} ({})".format(name, target),
                      ("call_bound", name, target))
        else:
            target = self.pick_string()
            result = self.fresh("x")
            self.ints.append(result)
            self.emit("val {} = string_length (borrow {})".format(
                result, target), ("use_str", target))

    def source(self):
        body = "\n".join("        " + line for line in self.lines)
        result = " + ".join(self.ints) if self.ints else "0"
        return (FAMILY4_PRELUDE +
                "\nfun run (c0 : bool, c1 : bool, c2 : bool) : int =\n"
                "    let\n" + body + "\n    in " + result + "\n    end\n" + MAIN)

    def verdict(self):
        values = set()
        moved = set()

        def need(name):
            if name not in values or name in moved:
                raise Wrong("use of moved " + name)

        try:
            for effect in self.effects:
                kind = effect[0]
                if kind == "new_str" or kind == "new_fun":
                    values.add(effect[1])
                elif kind == "apply_str":
                    need(effect[1])
                    moved.add(effect[1])
                elif kind == "call_bound":
                    _, name, target = effect
                    need(name)
                    need(target)
                    raise Wrong(
                        "direct call to a borrowing lambda with an owned argument")
                elif kind == "use_str":
                    need(effect[1])
        except Wrong as error:
            return [str(error)]
        return []


def generate_callback(seed):
    rng = random.Random(seed)
    gen = CallbackGen(rng)
    for _ in range(2):
        name = gen.fresh("s")
        gen.strings.append(name)
        gen.emit('val {} = "{}" ^ "!"'.format(name, name), ("new_str", name))
    for _ in range(rng.randint(4, 9)):
        gen.statement()
    return gen


TAIL_MOD = 1000003
TAIL_DEPTH = 200000

TAIL_CALL_DEPTH = 2000

TAIL_PRELUDE = """type item = { name : string, k : int }

type cell = { value : int }

fun build_list (n : int, acc : int list) : int list =
    if n = 0 then acc else build_list (n - 1, Cons ((n % 5, acc)))

fun sum_list (values : borrow int list, total : int) : int =
    case values of
        Nil => total
      | Cons ((value, rest)) => sum_list (rest, (total + value) % 1000003)
"""




TAIL_CHAINS = {
    "tuple": ("""
datatype chain = End | Link of int * string * chain

fun build_chain (n : int, acc : chain) : chain =
    if n = 0 then acc
    else if n % 3 = 0 then build_chain (n - 1, Link ((n % 7, "a" ^ "", acc)))
    else build_chain (n - 1, Link ((n % 7, "bcd" ^ "", acc)))

fun chain_len (c : borrow chain, total : int) : int =
    case c of
        End => total
      | Link ((_, _, rest)) => chain_len (rest, total + 1)
""", "Link (({x}, {t}, {nx}))", 'Link (({x}, "f" ^ "", End))'),
    "record": ("""
type entry = { x : int, t : string }

datatype chain = End | Link of entry * chain

fun build_chain (n : int, acc : chain) : chain =
    if n = 0 then acc
    else if n % 3 = 0 then build_chain (n - 1, Link (({ x = n % 7, t = "a" ^ "" }, acc)))
    else build_chain (n - 1, Link (({ x = n % 7, t = "bcd" ^ "" }, acc)))

fun chain_len (c : borrow chain, total : int) : int =
    case c of
        End => total
      | Link ((_, rest)) => chain_len (rest, total + 1)
""", "Link (({{ x = {x}, t = {t} }}, {nx}))",
        'Link (({{ x = {x}, t = "f" ^ "" }}, End))'),
}



TAIL_PY_HELPERS = """
def build_list(n):
    acc = None
    for i in range(n, 0, -1):
        acc = (i % 5, acc)
    return acc

def sum_list(values):
    total = 0
    while values is not None:
        total = (total + values[0]) % 1000003
        values = values[1]
    return total

def build_chain(n):
    acc = None
    for i in range(n, 0, -1):
        acc = (i % 7, 1 if i % 3 == 0 else 3, acc)
    return acc

def chain_len(c):
    total = 0
    while c is not None:
        total += 1
        c = c[2]
    return total
"""


def tail_indent(lines, by=4):
    return [" " * by + line for line in lines]


def tail_paren(lines):
    return ["("] + tail_indent(lines) + [")"]


class TailGen:
    """Family `tail`: one self-tail-recursive `loop` run to depth 200 000.

    Its parameters, each present at random: `n`, `acc`, an owned string
    `s`, a record `r : item` whose `name` is often moved out (the drop
    keeps a `skip` path), an owned list `l` consumed by a `case`, a
    borrowed string `b`, a borrow root `w` (a chain walked by a
    constructor pattern whose tuple or record payload binds `x`, the
    boxed `t` and the next cell) and a borrowed record `p`.  The body is
    a random tree of `if`, `case` and `let` whose leaves are self calls
    that rebuild every argument; the result is a checksum.

    The generator writes the same tree in Python (`py` lines; a string is
    its length, a list or chain a nested tuple) and runs it for the
    expected output.  Every argument form is in the classes that
    `LlvmTail` accepts, so the self call must become a loop; a `poison`
    program instead passes a borrow of a local at every leaf (`p`: a record
    with no drop, which only `safeTailArgument` stops; `b`, `w`: values with
    a pending drop), so the self call must stay, at depth 2000."""

    def __init__(self, rng):
        self.rng = rng
        self.counter = 0
        has = lambda: rng.random() < 0.6
        self.has_s = has()
        self.has_r = has()
        self.has_l = has()
        self.has_b = has()
        self.has_w = has()
        self.has_p = has()
        self.chain = rng.choice(sorted(TAIL_CHAINS))
        self.poison = None
        if rng.random() < 0.25:
            self.poison = rng.choice(["cell", "cell", "string", "chain"])
            if self.poison == "cell":
                self.has_p = True
            elif self.poison == "string":
                self.has_b = True
            else:
                self.has_w = True
        self.loop = self.poison is None
        self.depth = TAIL_DEPTH if self.loop else TAIL_CALL_DEPTH
        self.s0 = "s" * rng.randint(1, 5)
        self.rn0 = "r" * rng.randint(1, 5)
        self.l0 = rng.randint(0, 5)
        self.b0 = "b" * rng.randint(1, 6)
        self.w0 = rng.choice([self.depth + 1, self.depth // 3, rng.randint(0, 50)])
        self.p0 = rng.randint(0, 6)
        ctx = {"l": "whole" if self.has_l else None,
               "w": "whole" if self.has_w else None,
               "short": None, "plain": [], "lets": []}
        self.body, self.py = self.node(ctx, 0)

    def fresh(self, prefix):
        self.counter += 1
        return "{}{}".format(prefix, self.counter)


    def plain_atoms(self, ctx):
        """Atoms that move nothing: usable in the arguments of the call."""
        k = self.rng.choice([2, 3, 5, 7])
        atoms = [("n % {}".format(k), "n % {}".format(k))]
        if self.has_b:
            atoms.append(("string_length (b)", "b"))
        if self.has_p:
            atoms.append(("p.value", "p"))
        return atoms + ctx["plain"]

    def read_atoms(self, ctx):
        """Plain atoms plus reads of `s` and `r`, before anything moves."""
        atoms = self.plain_atoms(ctx)
        if self.has_s:
            atoms.append(("string_length (borrow s)", "s"))
        if self.has_r:
            atoms += [("r.k", "rk"), ("string_length (borrow r.name)", "rn")]
        return atoms

    def cond(self, atoms):
        a = self.rng.choice(atoms)
        roll = self.rng.random()
        if roll < 0.4:
            return ("({}) % 2 = 0".format(a[0]), "({}) % 2 == 0".format(a[1]))
        c = self.rng.randint(0, 6)
        op = ">" if roll < 0.7 else "<"
        return ("{} {} {}".format(a[0], op, c), "{} {} {}".format(a[1], op, c))

    def int_expr(self, atoms):
        a = self.rng.choice(atoms)
        b = self.rng.choice(atoms)
        roll = self.rng.random()
        if roll < 0.35:
            return a
        if roll < 0.6:
            return ("{} + {}".format(a[0], b[0]), "{} + {}".format(a[1], b[1]))
        if roll < 0.75:
            return ("({}) * 3 + {}".format(a[0], b[0]),
                    "({}) * 3 + {}".format(a[1], b[1]))
        c = self.cond(atoms)
        return ("(if {} then {} else {})".format(c[0], a[0], b[0]),
                "({} if {} else {})".format(a[1], c[1], b[1]))

    def int_let(self, ctx, atoms, ylets, plets):
        name = self.fresh("y")
        e = self.int_expr(atoms)
        ylets.append("val {} = {}".format(name, e[0]))
        plets.append("{} = {}".format(name, e[1]))
        ctx["plain"].append((name, name))
        ctx["lets"].append(name)


    def node(self, ctx, depth):
        ctx = dict(ctx, plain=list(ctx["plain"]), lets=list(ctx["lets"]))
        ylets, plets = [], []
        for _ in range(self.rng.choice([0, 0, 1, 1, 2])):
            self.int_let(ctx, self.read_atoms(ctx), ylets, plets)
        options = ["leaf"] * (1 + 2 * depth)
        if depth < 3:
            options += ["if", "if"]
            if ctx["l"] == "whole":
                options += ["case_l"] * 3
            if ctx["w"] == "whole":
                options += ["case_w"] * 3
            if self.has_s and ctx["short"] is None:
                options.append("short")
        kind = self.rng.choice(options)
        if kind == "leaf":
            body, py = self.leaf(ctx, ylets, plets)
            return body, py
        if kind in ("if", "short"):
            if kind == "if":
                c = self.cond(self.read_atoms(ctx))
                yes, no = ctx, ctx
            else:
                c = ("string_length (borrow s) < 6", "s < 6")
                yes, no = dict(ctx, short=True), dict(ctx, short=False)
            ty, tp = self.node(yes, depth + 1)
            ey, ep = self.node(no, depth + 1)
            body = (["if {} then".format(c[0])] + tail_indent(tail_paren(ty)) +
                    ["else"] + tail_indent(tail_paren(ey)))
            py = (["if {}:".format(c[1])] + tail_indent(tp) +
                  ["else:"] + tail_indent(ep))
        elif kind == "case_l":
            v, tl = self.fresh("v"), self.fresh("tl")
            ny, np_ = self.node(dict(ctx, l="nil"), depth + 1)
            cons = dict(ctx, l=("cons", v, tl), plain=ctx["plain"] + [(v, v)])
            cy, cp = self.node(cons, depth + 1)
            body = (["case l of", "    Nil =>"] + tail_indent(tail_paren(ny), 8) +
                    ["  | Cons (({}, {})) =>".format(v, tl)] +
                    tail_indent(tail_paren(cy), 8))
            py = (["if l is None:"] + tail_indent(np_) +
                  ["else:", "    {}, {} = l".format(v, tl)] + tail_indent(cp))
        else:
            x, t, nx = self.fresh("x"), self.fresh("t"), self.fresh("nx")
            ey, ep = self.node(dict(ctx, w="end"), depth + 1)
            link = dict(ctx, w=("link", x, t, nx),
                        plain=ctx["plain"] + [(x, x), ("string_length ({})".format(t), t)])
            ly, lp = self.node(link, depth + 1)
            pattern = TAIL_CHAINS[self.chain][1].format(x=x, t=t, nx=nx)
            body = (["case w of", "    End =>"] + tail_indent(tail_paren(ey), 8) +
                    ["  | {} =>".format(pattern)] + tail_indent(tail_paren(ly), 8))
            py = (["if w is None:"] + tail_indent(ep) +
                  ["else:", "    {}, {}, {} = w".format(x, t, nx)] + tail_indent(lp))
        return self.wrap(ylets, body), plets + py

    @staticmethod
    def wrap(ylets, body):
        if not ylets:
            return body
        return (["let"] + tail_indent(ylets) + ["in"] + tail_indent(body) +
                ["end"])

    def leaf(self, ctx, ylets, plets):
        """A self call; `ylets`/`plets` are the node's lets so far."""
        rng = self.rng
        ctx = dict(ctx, plain=list(ctx["plain"]), lets=list(ctx["lets"]))
        args = {}

        def plain10():
            e = self.int_expr(self.plain_atoms(ctx))
            return ("({}) % 10".format(e[0]), "({}) % 10".format(e[1]))



        if self.poison == "cell":
            loc = self.fresh("loc")
            e = self.int_expr(self.plain_atoms(ctx))

            ylets.append("val {} = {{ value = (n + {}) % 7 }}".format(loc, e[0]))
            plets.append("{} = (n + {}) % 7".format(loc, e[1]))
            args["p"] = ("borrow " + loc, loc)
            self.int_let(ctx, [("p.value", "p")], ylets, plets)
        elif self.poison == "string":
            tmp = self.fresh("tmp")
            e = plain10()
            ylets.append('val {} = "q" ^ int_to_string ({})'.format(tmp, e[0]))
            plets.append("{} = 2".format(tmp))
            args["b"] = ("borrow " + tmp, tmp)
        elif self.poison == "chain":
            fr = self.fresh("fr")
            e = self.int_expr(self.plain_atoms(ctx))
            x = "({}) % 7".format(e[0])
            ylets.append("val {} = {}".format(fr, TAIL_CHAINS[self.chain][2].format(x=x)))
            plets.append("{} = (({}) % 7, 1, None)".format(fr, e[1]))
            args["w"] = ("borrow " + fr, fr)

        for _ in range(rng.choice([0, 1, 1, 2])):
            self.int_let(ctx, self.read_atoms(ctx), ylets, plets)


        if self.has_r:
            k = self.fresh("k")
            e = self.int_expr(self.read_atoms(ctx))
            ylets.append("val {} = (r.k + {}) % 1000".format(k, e[0]))
            plets.append("{} = (rk + {}) % 1000".format(k, e[1]))


        s_opts = []
        if self.has_s:
            s_opts = ["keep", "fresh", "let"]
            if ctx["short"]:
                s_opts += ["grow", "letgrow"]
        r_opts = []
        if self.has_r:
            r_opts = ["keep", "skip", "letname", "fresh", "letrec"]
            if self.has_s:
                r_opts += ["swap", "letswap"]
        r_choice = rng.choice(r_opts) if r_opts else None
        if r_choice in ("swap", "letswap"):
            s_choice = None
        else:
            s_choice = rng.choice(s_opts) if s_opts else None

        if s_choice == "keep":
            args["s"] = ("s", "s")
        elif s_choice == "fresh":
            e = plain10()
            args["s"] = ('"k" ^ int_to_string ({})'.format(e[0]), "2")
        elif s_choice in ("let", "letgrow"):
            name = self.fresh("s")
            grow = s_choice == "letgrow"
            ylets.append('val {} = s ^ "{}"'.format(name, "z" if grow else ""))
            plets.append("{} = s{}".format(name, " + 1" if grow else ""))
            args["s"] = (name, name)
        elif s_choice == "grow":
            args["s"] = ('s ^ "z"', "s + 1")

        if r_choice == "keep":
            args["r"] = ("r", "rn", "rk")
        elif r_choice == "skip":
            args["r"] = ("{{ name = r.name, k = {} }}".format(k), "rn", k)
        elif r_choice == "letname":
            nm = self.fresh("nm")
            ylets.append("val {} = r.name".format(nm))
            plets.append("{} = rn".format(nm))
            args["r"] = ("{{ name = {}, k = {} }}".format(nm, k), nm, k)
        elif r_choice == "fresh":
            e = plain10()
            args["r"] = ('{{ name = "n" ^ int_to_string ({}), k = {} }}'.format(e[0], k),
                         "2", k)
        elif r_choice == "letrec":
            rec = self.fresh("rec")
            ylets.append("val {} = {{ name = r.name, k = {} }}".format(rec, k))
            plets.append("{}n = rn".format(rec))
            args["r"] = (rec, rec + "n", k)
        elif r_choice == "swap":
            args["s"] = ("r.name", "rn")
            args["r"] = ("{{ name = s, k = {} }}".format(k), "s", k)
        elif r_choice == "letswap":
            nm = self.fresh("nm")
            ylets.append("val {} = r.name".format(nm))
            plets.append("{} = rn".format(nm))
            args["s"] = (nm, nm)
            args["r"] = ("{{ name = s, k = {} }}".format(k), "s", k)

        state = ctx["l"]
        if state == "whole":
            if rng.random() < 0.5:
                args["l"] = ("l", "l")
            else:
                e = plain10()
                args["l"] = ("Cons (({}, l))".format(e[0]), "({}, l)".format(e[1]))
        elif state == "nil":
            roll = rng.random()
            if roll < 0.25:
                args["l"] = ("Nil", "None")
            elif roll < 0.5:
                args["l"] = ("build_list (3, Nil)", "build_list(3)")
            else:
                e = plain10()
                ctor = ("Cons (({}, Nil))".format(e[0]), "({}, None)".format(e[1]))
                args["l"] = self.maybe_let(ctor, ylets, plets)
        elif state is not None:
            _, v, tl = state
            roll = rng.random()
            if roll < 0.3:
                args["l"] = (tl, tl)
            else:
                e = plain10()
                if roll < 0.65:
                    ctor = ("Cons (({}, {}))".format(e[0], tl), "({}, {})".format(e[1], tl))
                else:
                    ctor = ("Cons (({}, Cons (({}, {}))))".format(e[0], v, tl),
                            "({}, ({}, {}))".format(e[1], v, tl))
                args["l"] = self.maybe_let(ctor, ylets, plets)

        if self.has_b and "b" not in args:
            w = ctx["w"]
            if isinstance(w, tuple) and rng.random() < 0.6:
                args["b"] = (w[2], w[2])
            else:
                args["b"] = ("b", "b")
        if self.has_w and "w" not in args:
            w = ctx["w"]
            if isinstance(w, tuple) and rng.random() < 0.8:
                args["w"] = (w[3], w[3])
            else:
                args["w"] = ("w", "w")
        if self.has_p and "p" not in args:
            args["p"] = ("p", "p")

        e = self.int_expr(self.plain_atoms(ctx))
        acc_y = "(" + " + ".join(["acc"] + ctx["lets"] + [e[0]]) + ") % 1000003"
        acc_p = "(" + " + ".join(["acc"] + ctx["lets"] + [e[1]]) + ") % 1000003"
        yargs, pnames, pvals = ["n - 1", acc_y], ["n", "acc"], ["n - 1", acc_p]
        for name in ("s", "r", "l", "b", "w", "p"):
            if name not in args:
                continue
            value = args[name]
            yargs.append(value[0])
            if name == "r":
                pnames += ["rn", "rk"]
                pvals += [value[1], value[2]]
            else:
                pnames.append(name)
                pvals.append(value[1])
        call = "loop ({})".format(", ".join(yargs))
        py = plets + ["{} = {}".format(", ".join(pnames), ", ".join(pvals)),
                      "continue"]
        return self.wrap(ylets, [call]), py

    def maybe_let(self, ctor, ylets, plets):
        if self.rng.random() < 0.3:
            name = self.fresh("l")
            ylets.append("val {} = {}".format(name, ctor[0]))
            plets.append("{} = {}".format(name, ctor[1]))
            return (name, name)
        return ctor


    def source(self):
        params = ["n : int", "acc : int"]
        final = ["acc"]
        if self.has_s:
            params.append("s : string")
            final.append("string_length (borrow s)")
        if self.has_r:
            params.append("r : item")
            final += ["r.k", "string_length (borrow r.name)"]
        if self.has_l:
            params.append("l : int list")
            final.append("sum_list (borrow l, 0)")
        if self.has_b:
            params.append("b : borrow string")
            final.append("string_length (b)")
        if self.has_w:
            params.append("w : borrow chain")
            final.append("chain_len (w, 0)")
        if self.has_p:
            params.append("p : borrow cell")
            final.append("p.value")
        head = ("fun loop ({}) : int =\n    if n <= 0 then ({}) % 1000003\n    else\n"
                .format(", ".join(params), " + ".join(final)))
        body = "\n".join(tail_indent(self.body, 8))
        lets, args = [], [str(self.depth), "0"]
        if self.has_s:
            args.append('"{}" ^ ""'.format(self.s0))
        if self.has_r:
            args.append('{{ name = "{}" ^ "", k = 0 }}'.format(self.rn0))
        if self.has_l:
            args.append("build_list ({}, Nil)".format(self.l0))
        if self.has_b:
            lets.append('val text = "{}"'.format(self.b0))
            args.append("borrow text")
        if self.has_w:
            lets.append("val chain = build_chain ({}, End)".format(self.w0))
            args.append("borrow chain")
        if self.has_p:
            lets.append("val start = {{ value = {} }}".format(self.p0))
            args.append("borrow start")
        call = "print_int (loop ({}))".format(", ".join(args))
        if lets:
            main = ("    let\n" + "\n".join("        " + x for x in lets) +
                    "\n    in " + call + "\n    end\n")
        else:
            main = "    " + call + "\n"
        return (TAIL_PRELUDE + TAIL_CHAINS[self.chain][0] + "\n" + head + body +
                "\n\nfun main () =\n" + main)

    def python(self):
        init = ["n = {}".format(self.depth), "acc = 0"]
        final = ["acc"]
        if self.has_s:
            init.append("s = {}".format(len(self.s0)))
            final.append("s")
        if self.has_r:
            init += ["rn = {}".format(len(self.rn0)), "rk = 0"]
            final += ["rk", "rn"]
        if self.has_l:
            init.append("l = build_list({})".format(self.l0))
            final.append("sum_list(l)")
        if self.has_b:
            init.append("b = {}".format(len(self.b0)))
            final.append("b")
        if self.has_w:
            init.append("w = build_chain({})".format(self.w0))
            final.append("chain_len(w)")
        if self.has_p:
            init.append("p = {}".format(self.p0))
            final.append("p")
        lines = (["def run():"] + tail_indent(init) +
                 ["    while True:", "        if n <= 0:",
                  "            return ({}) % 1000003".format(" + ".join(final))] +
                 tail_indent(self.py, 8))
        return TAIL_PY_HELPERS + "\n" + "\n".join(lines) + "\n"

    def expected(self):
        space = {}
        exec(self.python(), space)
        return str(space["run"]())


def generate_tail(seed):
    return TailGen(random.Random(seed))


FAMILIES = ("baseline", "case-datatype", "borrow-mut", "containers",
            "partial-move", "generics", "guard", "callbacks", "tail")


def generate(seed, family="baseline"):
    if family == "baseline":
        return generate_baseline(seed)
    if family == "case-datatype":
        return generate_case(seed)
    if family == "borrow-mut":
        return generate_borrow(seed)
    if family == "containers":
        return generate_container(seed)
    if family == "partial-move":
        return generate_nested(seed)
    if family == "generics":
        return generate_generic(seed)
    if family == "guard":
        return generate_guard(seed)
    if family == "callbacks":
        return generate_callback(seed)
    if family == "tail":
        return generate_tail(seed)
    raise SystemExit("unknown family: " + family)


def generate_baseline(seed):
    rng = random.Random(seed)
    gen = Gen(rng)
    for prefix, text in (("s", '"{}" ^ "!"'), ("s", '"{}" ^ "?"')):
        name = gen.fresh(prefix)
        gen.kinds[name] = "str"
        gen.emit("val {} = {}".format(name, text.format(name)),
                 lambda st, c, n=name: st.__setitem__(n, {"moved": False}))
    name = gen.fresh("r")
    gen.kinds[name] = "item"
    gen.emit('val {} : item = {{ name = "{}" ^ "#", k = 1 }}'.format(name, name),
             lambda st, c, n=name: st.__setitem__(n, {"moved": False}))
    if rng.random() < 0.6:
        name = gen.fresh("t")
        gen.kinds[name] = "tok"
        gen.emit("val {} = new_token ()".format(name),
                 lambda st, c, n=name: st.__setitem__(n, {"moved": False}))
    for _ in range(rng.randint(3, 12)):
        gen.statement()

    if rng.random() < 0.8:
        for name, kind in list(gen.kinds.items()):
            if kind == "tok" and name in gen.alive_everywhere():
                gen.emit("val _ = consume_token ({})".format(name),
                         lambda st, c, t=name: Gen.move(st, t))
    return gen


def sh(args, env=None, cwd=None, timeout=60):
    full = dict(os.environ)
    full.update(env or {})
    return subprocess.run(args, env=full, cwd=cwd, capture_output=True,
                          text=True, timeout=timeout)


def codes(text):
    import re
    return sorted(set(re.findall(r"error\[(E\d+)\]", text)))


def judge(seed, family="baseline"):
    gen = generate(seed, family)
    wrong = gen.verdict()
    src = gen.source()
    with tempfile.TemporaryDirectory() as tmp:
        path = os.path.join(tmp, "case.yack")
        with open(path, "w") as f:
            f.write(src)
        nll = codes(sh([YACK, "check", path]).stderr)
        nll = [c for c in nll if c != "E0900" and c != "E0901"]
        if any(not c.startswith(("E04", "E05")) for c in nll):
            return {"seed": seed, "src": src, "invalid": nll}
        graph_out = sh([YACK, "check", path], {"YACK_BORROW_BYPASS": "1"}).stderr
        graph = [c for c in codes(graph_out) if c in ("E0900", "E0901")]
        lowering = [c for c in codes(graph_out) if c not in ("E0900", "E0901")]
        asan = ""
        ll = sh([YACK, "llvm", path],
                {"YACK_BORROW_BYPASS": "1", "YACK_NO_VALIDATE": "1"})
        if ll.returncode == 0 and ll.stdout:
            with open(path + ".ll", "w") as f:
                f.write(ll.stdout)
            exe = os.path.join(tmp, "case")
            cc = sh(["clang", "-O1", "-g", "-fsanitize=address",
                     "-Wno-override-module", "-o", exe, path + ".ll"])
            if cc.returncode == 0:
                run = sh([exe], {"ASAN_OPTIONS": "detect_leaks=1"}, timeout=20)
                for line in run.stderr.splitlines():
                    if "ERROR: AddressSanitizer" in line or "ERROR: LeakSanitizer" in line:
                        asan = line.split("ERROR: ")[1].split(" on ")[0]
                        break
            else:
                asan = "clang-failed"
        else:
            asan = "no-lowering"
    return {"seed": seed, "src": src, "wrong": wrong, "nll": nll,
            "graph": graph, "lowering": lowering, "asan": asan}


def loop_self_calls(llvm):
    """Self calls left in `@yack_loop`, or None if it is missing."""
    body = None
    for line in llvm.splitlines():
        if body is None:
            if line.startswith("define ") and "@yack_loop(" in line:
                body = []
        elif line == "}":
            break
        else:
            body.append(line)
    if body is None:
        return None
    return sum(1 for line in body if "call " in line and "@yack_loop(" in line)


def small_stack():
    import resource
    resource.setrlimit(resource.RLIMIT_STACK, (8 << 20, 8 << 20))


def judge_tail(seed):
    """Heap under ASan (`--arena=noop`, an 8 MB stack), stack mode at -O0
    (8 MB), and the self call in the `.ll`: gone for a loop program, still
    there for a poison one.  Returns the list of problems."""
    gen = generate_tail(seed)
    src = gen.source()
    problems = []
    with tempfile.TemporaryDirectory() as tmp:
        path = os.path.join(tmp, "case.yack")
        with open(path, "w") as f:
            f.write(src)
        flags = {"heap": ["--memory=heap", "--arena=noop"],
                 "stack": ["--memory=stack"]}
        llvm = {mode: subprocess.Popen([YACK, "llvm"] + flags[mode] + [path],
                                       stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                       text=True)
                for mode in flags}

        expected = gen.expected()
        lowered = {}
        for mode, proc in llvm.items():
            out, err = proc.communicate(timeout=120)
            if proc.returncode != 0 or not out:
                first = [x for x in err.splitlines() if "error" in x][:1]
                problems.append("{}-llvm: {}".format(mode, first[0] if first else err[:200]))
                continue
            lowered[mode] = out
            calls = loop_self_calls(out)
            if calls is None:
                problems.append("{}: no @yack_loop".format(mode))
            elif gen.loop and calls:
                problems.append("{}: {} self call(s) left".format(mode, calls))
            elif not gen.loop and not calls:
                problems.append("{}: self call became a loop".format(mode))
        builds = {"heap": ["-O1", "-g", "-fsanitize=address"], "stack": ["-O0"]}
        for mode, out in lowered.items():
            ll = os.path.join(tmp, mode + ".ll")
            exe = os.path.join(tmp, mode)
            with open(ll, "w") as f:
                f.write(out)
            cc = sh(["clang"] + builds[mode] + ["-Wno-override-module", "-o", exe, ll],
                    timeout=120)
            if cc.returncode != 0:
                problems.append("{}: clang failed".format(mode))
                continue
            try:
                run = subprocess.run([exe], capture_output=True, text=True, timeout=60,
                                     preexec_fn=small_stack,
                                     env=dict(os.environ, ASAN_OPTIONS="detect_leaks=1"))
            except subprocess.TimeoutExpired:
                problems.append("{}: timeout".format(mode))
                continue
            report = [x for x in run.stderr.splitlines()
                      if "ERROR: AddressSanitizer" in x or "ERROR: LeakSanitizer" in x]
            if report:
                problems.append("{}: {}".format(mode, report[0].split("ERROR: ")[1]))
            elif run.returncode != 0:
                problems.append("{}: exit {} {}".format(mode, run.returncode,
                                                         run.stderr.strip()[:200]))
            elif run.stdout.strip() != expected:
                problems.append("{}: printed {} instead of {}".format(
                    mode, run.stdout.strip(), expected))
    return {"seed": seed, "src": src, "loop": gen.loop, "poison": gen.poison,
            "problems": problems}


def run_tail_family(count, base):

    from concurrent.futures import ProcessPoolExecutor
    with ProcessPoolExecutor(max_workers=8) as pool:
        results = list(pool.map(judge_tail, range(base, base + count)))
    bad = 0
    for r in results:
        if not r["problems"]:
            continue
        bad += 1
        name = "tail-{}.yack".format(r["seed"])
        with open(os.path.join(OUT, name), "w") as f:
            f.write("(* {} *)\n".format("; ".join(r["problems"])))
            f.write(r["src"])
        print("FAIL seed {}: {}".format(r["seed"], "; ".join(r["problems"])))
    loops = sum(1 for r in results if r["loop"])
    print("cases: {} loop (depth {}), {} poison call (depth {})".format(
        loops, TAIL_DEPTH, len(results) - loops, TAIL_CALL_DEPTH))
    print("failures: {}".format(bad))
    print("cases written to " + OUT)
    return bad


def run_family(family, count, base):
    if family == "tail":
        return run_tail_family(count, base)
    with ThreadPoolExecutor(max_workers=8) as pool:
        results = list(pool.map(lambda seed: judge(seed, family),
                                range(base, base + count)))

    table = {}
    bad = 0
    invalid = [r for r in results if "invalid" in r]
    for r in invalid:
        path = os.path.join(OUT, "{}-invalid_{}.yack".format(family, r["seed"]))
        with open(path, "w") as f:
            f.write("(* {} *)\n".format(r["invalid"]) + r["src"])
    results = [r for r in results if "invalid" not in r]
    for r in results:
        truth = "wrong" if r["wrong"] else "right"
        nll = "reject" if r["nll"] else "accept"
        if r["graph"]:
            graph = "reject"
        elif r["lowering"]:
            graph = "lowering"
        else:
            graph = "accept"
        memory = "asan" if r["asan"] and r["asan"] not in ("no-lowering", "clang-failed") else "clean"
        key = (truth, nll, graph, memory)
        table[key] = table.get(key, 0) + 1
        notes = []
        if truth == "wrong" and nll == "accept":
            notes.append("NLL-unsound")
        if truth == "wrong" and graph == "accept":
            notes.append("graph-unsound")
        if truth == "right" and nll == "reject":
            notes.append("NLL-imprecise")
        if truth == "right" and graph != "accept":
            notes.append("graph-imprecise")
        if truth == "right" and memory == "asan":
            notes.append("asan-on-right")
        if "NLL-unsound" in notes or "asan-on-right" in notes:
            bad += 1
        if notes:
            name = "{}-{}_{}.yack".format(family, "_".join(notes), r["seed"])
            with open(os.path.join(OUT, name), "w") as f:
                f.write("(* truth: {}\n   nll: {}  graph: {}  lowering: {}  asan: {} *)\n".format(
                    "; ".join(r["wrong"]) or "right", r["nll"], r["graph"],
                    r["lowering"], r["asan"] or "clean"))
                f.write(r["src"])

    print("truth  nll     graph     memory  count")
    for key in sorted(table):
        print("{:6} {:7} {:9} {:7} {}".format(*key, table[key]))

    def count(pred):
        return sum(v for k, v in table.items() if pred(*k))

    wrong = count(lambda t, n, g, m: t == "wrong")
    right = count(lambda t, n, g, m: t == "right")
    print()
    print("cases: {} wrong, {} right".format(wrong, right))
    print("NLL   accepts wrong: {:4}   rejects right: {:4}".format(
        count(lambda t, n, g, m: t == "wrong" and n == "accept"),
        count(lambda t, n, g, m: t == "right" and n == "reject")))
    print("graph accepts wrong: {:4}   rejects right: {:4}   (lowering stops {} wrong, {} right)".format(
        count(lambda t, n, g, m: t == "wrong" and g == "accept"),
        count(lambda t, n, g, m: t == "right" and g != "accept"),
        count(lambda t, n, g, m: t == "wrong" and g == "lowering"),
        count(lambda t, n, g, m: t == "right" and g == "lowering")))
    print("ASan on right cases: {}".format(
        count(lambda t, n, g, m: t == "right" and m == "asan")))
    print("invalid (non-ownership error): {}".format(len(invalid)))
    print("cases written to " + OUT)
    return bad


def main():
    args = list(sys.argv[1:])
    family = "baseline"
    if "--family" in args:
        index = args.index("--family")
        family = args[index + 1]
        del args[index:index + 2]
    count = int(args[0]) if args else 400
    base = int(args[1]) if len(args) > 1 else 1
    os.makedirs(OUT, exist_ok=True)
    families = FAMILIES if family == "all" else (family,)
    bad = 0
    for name in families:
        if len(families) > 1:
            print("== family: {} ==".format(name))
        bad += run_family(name, count, base)
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
