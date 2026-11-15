structure Resolve =
struct
  datatype bindingKind =
      Function
    | Parameter
    | Local
    | Builtin
    | Constructor
    | Pattern
  datatype useKind = Read | Call

  type binding =
    { id : int
    , name : string
    , kind : bindingKind
    , span : Diag.span
    }

  type use =
    { binding : int
    , kind : useKind
    , span : Diag.span
    }

  type result =
    { bindings : binding list
    , uses : use list
    , diagnostics : Diag.t list
    }

  type state =
    { next : int ref
    , bindings : binding list ref
    , uses : use list ref
    , diagnostics : Diag.t list ref
    }

  fun fresh (state : state) =
    let
      val cell = #next state
      val id = !cell
      val _ = cell := id + 1
    in
      id
    end

  fun report (state : state) code message span notes hints =
    let val cell = #diagnostics state in
      cell := Diag.error (code, message, span, notes, hints) :: !cell
    end

  fun addBinding (state : state) name kind span =
    let
      val id = fresh state
      val binding = {id = id, name = name, kind = kind, span = span}
      val cell = #bindings state
      val _ = cell := binding :: !cell
    in
      id
    end

  fun addUse (state : state) binding kind span =
    let
      val use = {binding = binding, kind = kind, span = span}
      val cell = #uses state
    in
      cell := use :: !cell
    end

  fun lookupLocal name environment =
    case List.find (fn (candidate, _) => candidate = name) environment of
      SOME (_, id) => SOME id
    | NONE => NONE

  fun lookupFunction name functions =
    case List.find (fn (candidate, _) => candidate = name) functions of
      SOME (_, id) => SOME id
    | NONE => NONE

  fun resolveExpression (state : state, functions, environment, expression) =
    case expression of
      Ast.EInt _ => ()
    | Ast.EFloat _ => ()
    | Ast.EString _ => ()
    | Ast.EBool _ => ()
    | Ast.EUnit _ => ()

    | Ast.EVar (name, span) =>
        (case lookupLocal name environment of
           SOME id => addUse state id Read span
         | NONE =>
             (case lookupFunction name functions of
                SOME id => addUse state id Read span
              | NONE =>
                  report state
                    "E3001"
                    ("unknown variable `" ^ name ^ "`")
                    span
                    []
                    []))

    | Ast.ERecord (fields, _) =>
        let
          val _ =
            List.app
              (fn (_, value) =>
                resolveExpression (state, functions, environment, value))
              fields
        in
          ()
        end

    | Ast.EField (_, base, _) =>
        resolveExpression (state, functions, environment, base)

    | Ast.ETuple (values, _) =>
        let
          val _ =
            List.app
              (fn value =>
                resolveExpression (state, functions, environment, value))
              values
        in
          ()
        end

    | Ast.ETupleIndex (_, base, _) =>
        resolveExpression (state, functions, environment, base)

    | Ast.ECase (scrutinee, branches, _) =>
        let
          val _ =
            resolveExpression (state, functions, environment, scrutinee)
          fun resolveBranch (pattern, guard, body) =
            let
              val extended = resolvePattern (state, pattern, environment)
              val _ =
                case guard of
                  NONE => ()
                | SOME condition =>
                    resolveExpression (state, functions, extended, condition)
              val _ =
                resolveExpression (state, functions, extended, body)
            in
              ()
            end
          val _ = List.app resolveBranch branches
        in
          ()
        end

    | Ast.EBorrow (_, _, operand, _) =>
        resolveExpression (state, functions, environment, operand)

    | Ast.EFn (params, _, body, _) =>
        let
          val extended =
            List.foldl
              (fn (param, environment) =>
                let val id = addBinding state (#name param) Parameter (#span param)
                in ( #name param, id) :: environment end)
              environment
              params
        in
          resolveExpression (state, functions, extended, body)
        end

    | Ast.EExtern _ => ()

    | Ast.ECall (name, arguments, span) =>
        let
          val _ =
            List.app
              (fn argument =>
                resolveExpression (state, functions, environment, argument))
              arguments
        in
          case lookupLocal name environment of
            SOME id => addUse state id Read span
          | NONE =>
              (case lookupFunction name functions of
                 SOME id => addUse state id Call span
               | NONE =>
                   report state
                     "E3002"
                     ("unknown function `" ^ name ^ "`")
                     span
                     []
                     [])
        end

    | Ast.EBin (_, left, right, _) =>
        let
          val _ = resolveExpression (state, functions, environment, left)
          val _ = resolveExpression (state, functions, environment, right)
        in
          ()
        end

    | Ast.ENeg (operand, _) =>
        resolveExpression (state, functions, environment, operand)

    | Ast.EIf (condition, thenBranch, elseBranch, _) =>
        let
          val _ = resolveExpression (state, functions, environment, condition)
          val _ = resolveExpression (state, functions, environment, thenBranch)
          val _ = resolveExpression (state, functions, environment, elseBranch)
        in
          ()
        end

    | Ast.ELet (bindings, body, _) =>
        let
          fun addBindingToEnvironment (binding, environment) =
            case binding of
              Ast.BVal {name, value, span, ...} =>
                let
                  val _ =
                    resolveExpression (state, functions, environment, value)
                  val id = addBinding state name Local span
                in
                  (name, id) :: environment
                end
            | Ast.BPat {pattern, value, ...} =>
                let
                  val _ =
                    resolveExpression (state, functions, environment, value)
                in
                  resolvePattern (state, pattern, environment)
                end
            | Ast.BFun _ => raise Fail "resolve: desugared local function"
            | Ast.BGuard _ => raise Fail "resolve: desugared guard binding"
          val extended = List.foldl addBindingToEnvironment environment bindings
        in
          resolveExpression (state, functions, extended, body)
        end

    | Ast.EArena _ => raise Fail "resolve: desugared arena expression"

  and resolvePattern (state : state, pattern, environment) =
    case pattern of
      Ast.PVar (name, span) =>
        let val id = addBinding state name Pattern span
        in (name, id) :: environment end
    | Ast.PWildcard _ => environment
    | Ast.PInt _ => environment
    | Ast.PBool _ => environment
    | Ast.PString (_, rest, _) =>
        (case rest of
           SOME pattern => resolvePattern (state, pattern, environment)
         | NONE => environment)
    | Ast.PBytes (_, rest, _) =>
        (case rest of
           SOME pattern => resolvePattern (state, pattern, environment)
         | NONE => environment)
    | Ast.PConstructor (_, argument, _) =>
        (case argument of
           SOME pattern => resolvePattern (state, pattern, environment)
         | NONE => environment)
    | Ast.PTuple (patterns, _) =>
        List.foldl
          (fn (pattern, environment) =>
            resolvePattern (state, pattern, environment))
          environment
          patterns
    | Ast.PRecord (fields, _) =>
        List.foldl
          (fn ((_, pattern), environment) =>
            resolvePattern (state, pattern, environment))
          environment
          fields

  fun resolveFunction (state : state, functions, declaration) =
    case declaration of
      Ast.DFun {params, body, ...} =>
        let
          val environment =
            List.foldl
              (fn (param, environment) =>
                let val id = addBinding state (#name param) Parameter (#span param)
                in ( #name param, id) :: environment end)
              []
              params
        in
          resolveExpression (state, functions, environment, body)
        end
    | Ast.DType _ => ()
    | Ast.DAlias _ => ()
    | Ast.DDatatype _ => ()
    | Ast.DImport _ => ()
    | Ast.DExternRecord _ => ()
    | Ast.DFunClause _ => raise Fail "resolve: desugared function clause"

  fun resolve (program : Ast.program) : result =
    let
      val state : state =
        { next = ref 0
        , bindings = ref []
        , uses = ref []
        , diagnostics = ref []
        }
      val builtinSpan = {start = Pos.start, stop = Pos.start}
      val conversionNames =
        List.concat
          (List.map
             (fn width =>
               if width = 32 then
                 [ "int_to_string", "string_to_int" ]
               else
                 [ "int" ^ Int.toString width ^ "_to_string"
                 , "string_to_int" ^ Int.toString width
                 ])
             [1, 2, 4, 8, 16, 32, 64])
      val builtinFunctions =
        List.map
          (fn name => (name, addBinding state name Builtin builtinSpan))
          (["print_int", "print_int64", "print_float", "print_bool", "print_string", "int64_to_int", "int_to_int64", "int64_to_int_wrapping", "int64_wrapping_add", "int64_wrapping_sub", "int64_wrapping_mul", "float_to_string", "string_copy", "sprintf", "os.exit", "io.read_file", "io.read_stdin", "io.write_file", "string_length", "string_of_char", "string_of_int", "string_char_at", "string_slice", "string_eq", "string_char_at_unchecked", "string_find_char", "string_find_char_from", "print_span", "new_token", "consume_token", "new_resource", "drop_resource", "array_new", "array_make", "array_make_uninit", "array_length", "array_length_borrow", "array_get", "array_get_borrow_view", "array_get_copy_borrow", "array_get_unchecked", "array_get_borrow", "array_take", "array_put", "array_put_arena", "array_set", "array_set_unchecked", "array_push", "array_push_unchecked", "extern_view", "extern_view_ptr", "c_ptr.of_int", "c_ptr.is_null", "array_kernel_new", "array_kernel_length", "array_kernel_capacity", "array_kernel_load", "array_kernel_borrow", "array_kernel_take", "array_kernel_drop", "array_kernel_write", "array_kernel_reserve", "array_kernel_set_length", "array_kernel_bounds_abort", "map_new", "map_length", "map_length_borrow", "map_has", "map_has_borrow", "map_get", "map_get_borrow", "map_get_borrow_view", "map_get_copy_borrow", "map_set", "map_remove", "map_keys", "sqlite_open", "sqlite_close", "sqlite_exec", "sqlite_prepare", "sqlite_finalize", "sqlite_step", "sqlite_column_count", "sqlite_column_name", "sqlite_column_type", "sqlite_column_int", "sqlite_column_text", "sqlite_changes", "sqlite_errmsg", "bytes_from_string", "bytes_to_string", "bytes_to_string_range", "bytes_header_end", "bytes_write_string", "bytes_write_int", "recv_into", "send_from", "sys.socket_tcp", "sys.reuseaddr", "sys.reuseport", "sys.nodelay", "sys.bind_loopback", "sys.bind_any", "sys.listen", "sys.accept", "sys.connect_loopback", "sys.local_port", "sys.close", "sys.set_nonblocking", "sys.epoll_create", "sys.epoll_add", "sys.epoll_add_token", "sys.epoll_mod_token", "sys.epoll_del", "sys.epoll_wait", "sys.errno", "sys.install_stop_signals", "sys.stop_requested", "sys.sleep_ms", "sys.monotonic_ms", "sys.realtime_ms", "sys.scratch_enter", "sys.scratch_enter_all", "sys.scratch_leave", "sys.thread_spawn", "sys.thread_join", "sys.recv_into_raw", "sys.send_string_raw", "sys.send_from_raw"] @ conversionNames)
      val userFunctions =
        List.foldl
          (fn (declaration, functions) =>
            case declaration of
              Ast.DFun {name, nameSpan, ...} =>
                (name, addBinding state name Function nameSpan) :: functions
            | Ast.DType _ => functions
            | Ast.DAlias _ => functions
            | Ast.DDatatype _ => functions
            | Ast.DImport _ => functions
            | Ast.DExternRecord _ => functions
            | Ast.DFunClause _ =>
                raise Fail "resolve: desugared function clause")
          []
          program
      val constructorBindings =
        List.foldl
          (fn (declaration, bindings) =>
            case declaration of
              Ast.DDatatype {constructors, span, ...} =>
                List.foldl
                  (fn ((constructor, _), bindings) =>
                    (constructor, addBinding state constructor Constructor span) ::
                    bindings)
                  bindings
                  constructors
            | _ => bindings)
          []
          program
      val functions =
        builtinFunctions @ List.rev userFunctions @ List.rev constructorBindings
      val _ =
        List.app
          (fn declaration =>
            case declaration of
              Ast.DDatatype _ => ()
            | _ => resolveFunction (state, functions, declaration))
          program
    in
      { bindings = List.rev (!(#bindings state))
      , uses = List.rev (!(#uses state))
      , diagnostics = List.map Diag.demangle (List.rev (!(#diagnostics state)))
      }
    end

  fun unusedFunctions ({bindings, uses, ...} : result) =
    let
      fun isUsed (binding : binding) =
        List.exists (fn use : use => #binding use = #id binding) uses
    in
      List.filter
        (fn binding : binding =>
          #kind binding = Function andalso not (isUsed binding))
        bindings
    end

  fun bindingKindName Function = "function"
    | bindingKindName Parameter = "parameter"
    | bindingKindName Local = "local"
    | bindingKindName Builtin = "builtin"
    | bindingKindName Constructor = "constructor"
    | bindingKindName Pattern = "pattern"

  fun useKindName Read = "read"
    | useKindName Call = "call"

  fun bindingSummary ({id, name, kind, span} : binding) =
    "#" ^ Int.toString id ^ " " ^ bindingKindName kind ^ " " ^ name ^
    " @ " ^ Pos.toString (#start span)

  fun useSummary ({binding, kind, span} : use) =
    useKindName kind ^ " #" ^ Int.toString binding ^ " @ " ^
    Pos.toString (#start span)
end
