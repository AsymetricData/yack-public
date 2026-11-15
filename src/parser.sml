structure Parser =
struct
  open ParserState

  type result =
    { program : Ast.program
    , diagnostics : Diag.t list

    , failed : string list
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
    val failed = ref []
    fun tokenAt index =
      if index < #count state then Array.sub (#tokens state, index)
      else Array.sub (#tokens state, #count state - 1)

    fun failedNameAt index =
      let
        fun firstIdent from =
          let
            fun loop i =
              if i >= #count state then NONE
              else
                case #kind (tokenAt i) of
                  Token.Ident name => SOME name
                | Token.TyVar _ => loop (i + 1)
                | Token.Punct _ => loop (i + 1)
                | _ => NONE
          in
            loop from
          end
        fun afterRec i =
          if Token.isKeyword "rec" (tokenAt i) then firstIdent (i + 1)
          else firstIdent i
        val start = tokenAt index
      in
        if Token.isKeyword "fun" start orelse Token.isKeyword "rec" start then
          afterRec (index + 1)
        else if Token.isKeyword "type" start orelse
                Token.isKeyword "datatype" start then
          firstIdent (index + 1)
        else
          case Token.identOf start of
            SOME "extern" =>
              if Token.isKeyword "fun" (tokenAt (index + 1)) then
                afterRec (index + 2)
              else if Token.identOf (tokenAt (index + 1)) = SOME "record" then
                firstIdent (index + 2)
              else firstIdent (index + 1)
          | _ => NONE
      end
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
              ( (case failedNameAt startIndex of
                   SOME name => failed := name :: !failed
                 | NONE => ())
              ; skipToDeclaration ()
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
    , failed = List.rev (!failed)
    }
  end
end
