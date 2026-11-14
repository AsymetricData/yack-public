structure LspTypes =
struct
  type field =
    { name : string
    , ty : string
    }

  datatype argument =
      NoArgument
    | RecordArgument of field list
    | TupleArgument of string list
    | OtherArgument of string

  type constructor =
    { name : string
    , typeName : string
    , argument : argument
    }

  type db =
    { records : (string * field list) list
    , datatypes : (string * constructor list) list
    , constructors : constructor list
    , fields : field list
    }

  fun fieldsOfSyntax syntax =
    case syntax of
      Ast.TyRecord entries =>
        List.map
          (fn (name, ty) => { name = name, ty = Ast.tyToString ty })
          entries
    | _ => []

  fun argumentOf syntax =
    case syntax of
      NONE => NoArgument
    | SOME (Ast.TyRecord entries) => RecordArgument (fieldsOfSyntax (Ast.TyRecord entries))
    | SOME (Ast.TyTuple types) => TupleArgument (List.map Ast.tyToString types)
    | SOME ty => OtherArgument (Ast.tyToString ty)

  fun patternText (constructor : constructor) =
    case #argument constructor of
      NoArgument => #name constructor
    | RecordArgument fields =>
        #name constructor ^ " ({" ^
        String.concatWith ", "
          (List.map (fn field => #name field ^ " = " ^ #name field) fields) ^
        "})"
    | TupleArgument types =>
        #name constructor ^ " ((" ^
        String.concatWith ", "
          (List.tabulate (List.length types, fn index =>
             "value" ^ Int.toString (index + 1))) ^
        "))"
    | OtherArgument _ => #name constructor ^ " (value)"

  fun insertField (field, fields) =
    if List.exists (fn candidate => #name candidate = #name field) fields then fields
    else field :: fields

  fun insertNamed (keyOf, item, items) =
    if List.exists (fn candidate => keyOf candidate = keyOf item) items then items
    else item :: items

  fun fromProgram program =
    let
      val records = ref []
      val datatypes = ref []
      val constructors = ref []
      val fields = ref []
      fun addFields entries =
        List.app
          (fn (name, ty) =>
            fields :=
              insertField ({ name = name, ty = Ast.tyToString ty }, !fields))
          entries
      val _ =
        List.app
          (fn declaration =>
            case declaration of
              Ast.DType {name, fields = entries, ...} =>
                let
                  val fieldList = fieldsOfSyntax (Ast.TyRecord entries)
                  val _ = addFields entries
                in
                  records :=
                    insertNamed (#1, (name, fieldList), !records)
                end
            | Ast.DDatatype {name, constructors = entries, ...} =>
                let
                  val infos =
                    List.map
                      (fn (constructor, argument) =>
                        { name = constructor
                        , typeName = name
                        , argument = argumentOf argument
                        })
                      entries
                  val _ =
                    List.app
                      (fn info =>
                        case #argument info of
                          RecordArgument recordFields =>
                            List.app
                              (fn field =>
                                fields :=
                                  insertField (field, !fields))
                              recordFields
                        | _ => ())
                      infos
                  val _ =
                    List.app
                      (fn info =>
                        constructors :=
                          insertNamed (#name, info, !constructors))
                      infos
                in
                  datatypes :=
                    insertNamed (#1, (name, infos), !datatypes)
                end
            | _ => ())
          program
    in
      { records = !records
      , datatypes = !datatypes
      , constructors = !constructors
      , fields = !fields
      }
    end

  fun merge (left : db, right : db) =
    { records = #records left @ #records right
    , datatypes = #datatypes left @ #datatypes right
    , constructors = #constructors left @ #constructors right
    , fields = #fields left @ #fields right
    }

  fun dbOf (modules : LspProject.indexModule list) program =
    List.foldl
      (fn (module, db) => merge (db, fromProgram (#program module)))
      (fromProgram program)
      modules

  fun findRecord (db : db) name =
    case List.find (fn (candidate, _) => candidate = name) (#records db) of
      SOME (_, fields) => SOME fields
    | NONE => NONE

  fun findDatatype (db : db) name =
    case List.find (fn (candidate, _) => candidate = name) (#datatypes db) of
      SOME (_, constructors) => SOME constructors
    | NONE => NONE

  fun findConstructor (db : db) name =
    List.find (fn constructor => #name constructor = name) (#constructors db)

  fun recordFieldsOf (db : db) constructor =
    case #argument constructor of
      RecordArgument fields => SOME fields
    | OtherArgument typeName => findRecord db typeName
    | TupleArgument _ => NONE
    | NoArgument => NONE

  fun patternTextWith (db : db) constructor =
    case recordFieldsOf db constructor of
      SOME fields =>
        #name constructor ^ " ({" ^
        String.concatWith ", "
          (List.map (fn field => #name field ^ " = " ^ #name field) fields) ^
        "})"
    | NONE => patternText constructor

  fun constructorSnippet (db : db) constructor =
    case recordFieldsOf db constructor of
      SOME fields =>
        let
          val placeholders =
            List.tabulate
              (List.length fields, fn index => "$" ^ Int.toString (index + 1))
        in
          #name constructor ^ " ({ " ^
          String.concatWith ", "
            (ListPair.map
               (fn (field, placeholder) =>
                 #name field ^ " = " ^ placeholder)
               (fields, placeholders)) ^
          " })"
        end
    | NONE => #name constructor

  fun allFields (db : db) = #fields db
  fun allConstructors (db : db) = #constructors db
end
