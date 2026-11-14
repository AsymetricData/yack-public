structure Parser =
struct
  open ParserState

  type result =
    { program : Ast.program
    , diagnostics : Diag.t list
    }

fun parse tokens =
  let
    val state : state =
      { tokens = Array.fromList tokens
      , count = List.length tokens
      , index = ref 0
      , diagnostics = ref []
      , allowArrowFun = ref true
      }
    val declarations = ref []
    fun skipToDeclaration () =
      if atEnd state orelse
         Token.isKeyword "fun" (current state) orelse
         Token.isKeyword "rec" (current state) orelse
         Token.isKeyword "type" (current state) orelse
         Token.isKeyword "datatype" (current state) orelse
         (case Token.identOf (current state) of
            SOME "import" => true
          | _ => false)
      then ()
      else (advance state; skipToDeclaration ())
    fun loop () =
      if atEnd state then ()
      else
        let val startIndex = !(#index state) in
          ( let val declaration = ParserBody.parseDecl state
                val _ = declarations := declaration :: !declarations
            in loop () end
            handle Abort =>
              ( skipToDeclaration ()
              ; if atEnd state then ()
                else if !(#index state) = startIndex then (advance state; loop ())
                else loop ()
              )
          )
        end
    val _ = loop ()
  in
    { program = List.rev (!declarations)
    , diagnostics = List.rev (!(#diagnostics state))
    }
  end
end
