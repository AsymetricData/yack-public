#!/usr/bin/env python3
"""Fuzz tail and non-tail recursion in heap and stack modes.

Each case is generated as a small Yack program with one deterministic answer.
The script checks:

- heap and stack builds succeed;
- heap and stack runs agree with the expected value and do not crash;
- a tail case has no self call left in its recursive function (the call
  became a jump to the loop header);
- a known non-tail case keeps its self call.

Run with `make test-fuzz` or `python3 test/fuzz_tail.py`.
"""
import os
import random
import re
import subprocess
import sys
import tempfile
from dataclasses import dataclass


YACK = os.path.abspath("./yack")


@dataclass
class Case:
    name: str
    source: str
    expected: str
    tail: bool | None


def program(body, main):
    return body + "\n\nfun main () =\n" + main + "\n"


def tail_int(n):
    body = """fun loop (n : int, acc : int) : int =
    if n <= 0 then acc else loop (n - 1, acc + n)"""
    return Case(
        "tail_int",
        program(body, f"    print_int (loop ({n}, 0))"),
        str(n * (n + 1) // 2),
        True,
    )


def tail_list(n):
    body = """fun build (n : int) : int list =
    if n <= 0 then Nil else Cons ((n, build (n - 1)))

fun sum (values : int list, acc : int) : int =
    case values of
        Nil => acc
      | value :: rest => sum (rest, acc + value)"""
    return Case(
        "tail_list",
        program(
            body,
            f"    let val values = build ({n})\n"
            f"    in print_int (sum (values, 0))\n"
            f"    end",
        ),
        str(n * (n + 1) // 2),
        True,
    )


def tail_string(n):
    body = """fun loop (text : string, n : int, acc : int) : int =
    if n <= 0 then acc + string_length (borrow text)
    else loop (text ^ "a", n - 1, acc + 1)"""
    return Case(
        "tail_string",
        program(body, f"    print_int (loop (\"\", {n}, 0))"),
        str(2 * n),
        True,
    )


def tail_array(n):
    body = """fun loop (values : int array, n : int, acc : int) : int =
    if n <= 0 then acc + array_length (values)
    else
        let val _ = array_set_unchecked (values, n - 1, n)
        in loop (values, n - 1, acc + 1)
        end"""
    return Case(
        "tail_array",
        program(
            body,
            f"    let val values : int array = array_make (0, {n})\n"
            f"    in print_int (loop (values, {n}, 0))\n"
            f"    end",
        ),
        str(2 * n),
        True,
    )


def tail_map(n):
    body = """fun loop (values : int map, n : int, acc : int) : int =
    if n <= 0 then acc + map_length (values)
    else
        let val _ = map_set (values, int_to_string (n), n)
        in loop (values, n - 1, acc + 1)
        end"""
    return Case(
        "tail_map",
        program(
            body,
            f"    let val values = map_new ()\n"
            f"    in print_int (loop (values, {n}, 0))\n"
            f"    end",
        ),
        str(2 * n),
        True,
    )


def tail_record_map(n):
    body = """type holder = { values : int map, remaining : int }

fun loop (state : holder, acc : int) : int =
    if state.remaining <= 0 then acc + map_length (state.values)
    else loop ({ values = state.values, remaining = state.remaining - 1 }, acc + 1)"""
    return Case(
        "tail_record_map",
        program(
            body,
            f"    let val values = map_new ()\n"
            f"        val state = {{ values = values, remaining = {n} }}\n"
            f"    in print_int (loop (state, 0))\n"
            f"    end",
        ),
        str(n),
        True,
    )


def tail_function(n):
    body = """fun inc (value : int) : int = value + 1

fun loop (f : int -> int, n : int) : int =
    if n <= 0 then f (0) else loop (f, n - 1)"""
    return Case(
        "tail_function",
        program(body, f"    print_int (loop (inc, {n}))"),
        "1",
        True,
    )


def non_tail_map_value(n):
    body = """datatype label : Copy = Label of int

fun loop (values : label map, n : int, acc : int) : int =
    if n <= 0 then acc else loop (values, n - 1, acc + 1)"""
    return Case(
        "non_tail_map_value",
        program(
            body,
            f"    let val values = map_new ()\n"
            f"        val _ = map_set (values, \"x\", Label (1))\n"
            f"    in print_int (loop (values, {n}, 0))\n"
            f"    end",
        ),
        str(n),
        False,
    )


def non_tail_tree(depth):
    body = """fun tree (n : int) : int =
    if n <= 0 then 0 else 1 + tree (n - 1) + tree (n - 1)"""
    return Case(
        "non_tail_tree",
        program(body, f"    print_int (tree ({depth}))"),
        str((1 << depth) - 1),
        False,
    )


def non_tail_mutual(depth):
    body = """fun even (n : int) : bool =
    if n <= 0 then true else odd (n - 1)

fun odd (n : int) : bool =
    if n <= 0 then false else even (n - 1)"""
    return Case(
        "non_tail_mutual",
        program(body, f"    print_bool (even ({depth}))"),
        "1" if depth % 2 == 0 else "0",
        None,
    )


def trap_ctor_current(n):
    body = """datatype box = Box of int

fun loop (value : box, n : int) : int =
    if n <= 0 then
        case value of
            Box (x) => x
    else
        let val next = Box (n)
        in loop (next, n - 1)
        end"""
    return Case(
        "trap_ctor_current",
        program(body, f"    print_int (loop (Box (0), {n}))"),
        "1",
        True,
    )


def trap_borrow_record(n):
    body = """type boxed = { value : int }

fun peek (value : borrow boxed) : int = value.value

fun loop (value : borrow boxed, n : int) : int =
    if n <= 0 then peek (value)
    else
        let val local = { value = n }
        in loop (borrow local, n - 1)
        end"""
    return Case(
        "trap_borrow_record",
        program(
            body,
            f"    let val start = {{ value = 0 }}\n"
            f"    in print_int (loop (borrow start, {n}))\n"
            f"    end",
        ),
        "1",
        False,
    )


def trap_borrow_string(n):
    body = """fun loop (text : borrow string, n : int) : int =
    if n <= 0 then string_length (text)
    else loop (text, n - 1)"""
    return Case(
        "trap_borrow_string",
        program(
            body,
            f"    let val text = \"abcdef\"\n"
            f"    in print_int (loop (borrow text, {n}))\n"
            f"    end",
        ),
        "6",
        True,
    )


def trap_stack_plan_dest(n):
    body = """datatype box = Box of int

fun make (n : int) : box = Box (n)

fun loop (n : int, acc : int) : int =
    if n <= 0 then acc
    else
        let val value = make (n)
            val added =
                case value of
                    Box (x) => x
        in loop (n - 1, acc + added)
        end"""
    return Case(
        "trap_stack_plan_dest",
        program(body, f"    print_int (loop ({n}, 0))"),
        str(n * (n + 1) // 2),
        None,
    )


def tail_return_map(n):
    body = """fun loop (values : int map, n : int) : int map =
    if n <= 0 then values else loop (values, n - 1)"""
    return Case(
        "tail_return_map",
        program(
            body,
            f"    let val values = map_new ()\n"
            f"        val _ = map_set (values, \"x\", 1)\n"
            f"    in print_int (map_length (loop (values, {n})))\n"
            f"    end",
        ),
        "1",
        True,
    )


def trap_apply_dest(n):
    body = """datatype box = Box of int

type maker = { run : int -> box }

fun make (n : int) : box = Box (n)

fun loop (m : maker, n : int, acc : int) : int =
    if n <= 0 then acc
    else
        let val f = m.run
            val value = f (n)
            val added = case value of Box (x) => x
        in loop (m, n - 1, acc + added)
        end"""
    return Case(
        "trap_apply_dest",
        program(
            body,
            f"    let val m = {{ run = make }}\n"
            f"    in print_int (loop (m, {n}, 0))\n"
            f"    end",
        ),
        str(n * (n + 1) // 2),
        None,
    )


def trap_nested_dest(n):
    body = """datatype box = Box of int

fun make (n : int) : box = Box (n)

fun run (n : int) : box = make (n)

fun loop (n : int, acc : int) : int =
    if n <= 0 then acc
    else
        let val value = run (n)
            val added = case value of Box (x) => x
        in loop (n - 1, acc + added)
        end"""
    return Case(
        "trap_nested_dest",
        program(body, f"    print_int (loop ({n}, 0))"),
        str(n * (n + 1) // 2),
        None,
    )


def cases(rng):
    n_tail = rng.choice([1000, 5000, 20000, 100000])
    return [
        tail_int(n_tail),
        tail_list(rng.choice([32, 64, 128])),
        tail_string(rng.choice([32, 64])),
        tail_array(rng.choice([16, 64, 256])),
        tail_map(rng.choice([16, 64, 256])),
        tail_record_map(rng.choice([16, 64, 256])),
        tail_function(rng.choice([16, 1000, 20000])),
        non_tail_map_value(rng.choice([16, 64])),
        non_tail_tree(rng.choice([8, 10, 12])),
        non_tail_mutual(rng.choice([16, 64, 256])),
        trap_ctor_current(rng.choice([16, 64])),
        trap_borrow_record(rng.choice([16, 64])),
        trap_borrow_string(rng.choice([1000, 20000])),
        trap_stack_plan_dest(rng.choice([1000, 5000])),
        tail_return_map(rng.choice([1000, 20000])),
        trap_apply_dest(rng.choice([16, 64])),
        trap_nested_dest(rng.choice([16, 64])),
    ]


FUNCTION_RE = re.compile(
    r"define [^\n]+ @([^\(]+)\([^\{]*\{(.*?)\n\}", re.S
)


def function_body(llvm, name):
    suffix = "_" + name
    for match in FUNCTION_RE.finditer(llvm):
        function = match.group(1)
        if (
            function == "yack_" + name
            or function.endswith(suffix)
            or suffix + "__" in function
        ):
            return match.group(2)
    return ""


def build(source, output, stack):
    args = [YACK, "build"]
    if stack:
        args.append("--memory=stack")
    args += [source, output]
    return subprocess.run(args, capture_output=True, text=True, timeout=60)


def run(binary):
    return subprocess.run(
        [binary], capture_output=True, text=True, timeout=20
    )


def main():
    rng = random.Random(int(os.environ.get("FUZZ_SEED", "1")))
    tmp = tempfile.mkdtemp(prefix="yack-fuzz-tail-")
    failures = 0
    try:
        for case in cases(rng):
            source = os.path.join(tmp, case.name + ".yack")
            with open(source, "w") as handle:
                handle.write(case.source)

            outputs = {}
            for mode in ("heap", "stack"):
                binary = os.path.join(tmp, case.name + "-" + mode)
                built = build(source, binary, mode == "stack")
                if built.returncode != 0:
                    print(f"FAIL {case.name} {mode} build")
                    print((built.stdout + built.stderr).strip()[:800])
                    failures += 1
                    break
                executed = run(binary)
                if executed.returncode != 0:
                    print(
                        f"FAIL {case.name} {mode} run "
                        f"(exit {executed.returncode})"
                    )
                    print((executed.stdout + executed.stderr).strip()[:800])
                    failures += 1
                    break
                outputs[mode] = executed.stdout.strip()
            else:
                if outputs["heap"] != outputs["stack"]:
                    print(
                        f"FAIL {case.name} heap/stack mismatch: "
                        f"{outputs['heap']!r} vs {outputs['stack']!r}"
                    )
                    failures += 1
                    continue
                if outputs["heap"] != case.expected:
                    print(
                        f"FAIL {case.name} expected {case.expected!r}, "
                        f"got {outputs['heap']!r}"
                    )
                    failures += 1
                    continue

                llvm = subprocess.run(
                    [YACK, "llvm", "--memory=stack", source],
                    capture_output=True,
                    text=True,
                    timeout=60,
                )
                if llvm.returncode != 0:
                    print(f"FAIL {case.name} llvm")
                    print((llvm.stdout + llvm.stderr).strip()[:800])
                    failures += 1
                    continue
                target = {
                    "tail_int": "loop",
                    "tail_list": "sum",
                    "tail_string": "loop",
                    "tail_array": "loop",
                    "tail_map": "loop",
                    "tail_record_map": "loop",
                    "tail_function": "loop",
                    "non_tail_map_value": "loop",
                    "non_tail_tree": "tree",
                    "non_tail_mutual": "even",
                    "trap_ctor_current": "loop",
                    "trap_borrow_record": "loop",
                    "trap_borrow_string": "loop",
                    "trap_stack_plan_dest": "loop",
                    "tail_return_map": "loop",
                    "trap_apply_dest": "loop",
                    "trap_nested_dest": "loop",
                }[case.name]
                body = function_body(llvm.stdout, target)
                has_self_call = ("@yack_" + target + "(") in body
                if case.tail is None:
                    print(f"ok   {case.name} (checked)")
                elif case.tail and has_self_call:
                    print(f"FAIL {case.name} self call left in {target}")
                    failures += 1
                elif not case.tail and not has_self_call:
                    print(f"FAIL {case.name} lost its self call in {target}")
                    failures += 1
                elif os.environ.get("FUZZ_ASAN"):
                    ll = os.path.join(tmp, case.name + ".ll")
                    asan = os.path.join(tmp, case.name + "-asan")
                    emitted = subprocess.run(
                        [YACK, "llvm", source],
                        capture_output=True,
                        text=True,
                        timeout=60,
                    )
                    if emitted.returncode != 0:
                        print(f"FAIL {case.name} asan llvm")
                        failures += 1
                        continue
                    with open(ll, "w") as handle:
                        handle.write(emitted.stdout)
                    compiled = subprocess.run(
                        [
                            "clang",
                            "-O1",
                            "-g",
                            "-fsanitize=address",
                            "-Wno-override-module",
                            "-o",
                            asan,
                            ll,
                        ],
                        capture_output=True,
                        text=True,
                        timeout=120,
                    )
                    if compiled.returncode != 0:
                        print(f"FAIL {case.name} asan compile")
                        failures += 1
                        continue
                    checked = subprocess.run(
                        [asan],
                        capture_output=True,
                        text=True,
                        timeout=60,
                        env={**os.environ, "ASAN_OPTIONS": "detect_leaks=0"},
                    )
                    if checked.returncode != 0:
                        print(f"FAIL {case.name} asan run")
                        print((checked.stdout + checked.stderr).strip()[:800])
                        failures += 1
                    else:
                        print(
                            f"ok   {case.name} "
                            f"({'tail' if case.tail else 'non-tail'}) +asan"
                        )
                else:
                    print(
                        f"ok   {case.name} "
                        f"({'tail' if case.tail else 'non-tail'})"
                    )
    finally:
        subprocess.run(["rm", "-rf", tmp])
    if failures:
        print(f"\n{failures} fuzz failure(s)")
        return 1
    print("\nall tail fuzz cases passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
