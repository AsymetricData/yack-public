structure LlvmData =
struct
  open LlvmTypes

  fun findData (datatypes : Ir.dataDef list, name) =
    List.find (fn definition => #name definition = name) datatypes

  fun constructorPayloadTypes (definition : Ir.dataDef) =
    List.mapPartial
      (fn (_, argument) => Option.map storageTy argument)
      (#constructors definition)

  fun dataStructTy (definition : Ir.dataDef) =
    "{ " ^
    String.concatWith ", "
      ("i32" :: "i1" :: constructorPayloadTypes definition) ^
    " }"

  fun constructorInfo (definition : Ir.dataDef) constructor =
    let
      fun loop (tag, payloadField, []) = NONE
        | loop (tag, payloadField, (name, argument) :: rest) =
            if name = constructor then
              SOME (tag, payloadField, argument)
            else
              loop
                ( tag + 1
                , case argument of SOME _ => payloadField + 1 | NONE => payloadField
                , rest
                )
    in
      loop (0, 2, #constructors definition)
    end
end
