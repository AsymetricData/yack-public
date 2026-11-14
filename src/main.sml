val main =
  let val arguments = CommandLine.arguments () in
    if arguments = ["lsp"] orelse arguments = ["lsp", "--stdio"] then Lsp.main ()
    else (Driver.main (); 0)
  end
