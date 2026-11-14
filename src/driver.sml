structure Driver =
struct
  type sourceEntry = DriverTypes.sourceEntry
  type checked = DriverTypes.checked
  type loadOptions = DriverTypes.loadOptions

  val diagnosticStyle = DriverTypes.diagnosticStyle
  val version = DriverUi.version

  val readFile = DriverIo.readFile
  val renderAll = DriverIo.renderAll
  val renderDiags = DriverIo.renderDiags

  val preludeText = DriverLoad.preludeText
  val loadWith = DriverLoad.loadWith
  val loadWithMode = DriverLoad.loadWithMode
  val requiresMain = DriverLoad.requiresMain
  val load = DriverLoad.load
  val check = DriverLoad.check
  val checkWithMode = DriverLoad.checkWithMode
  val manifestSettingsFor = DriverLoad.manifestSettingsFor

  val parseOnly = DriverCommands.parseOnly
  val showTokens = DriverCommands.showTokens
  val resolveOnly = DriverCommands.resolveOnly
  val arenaOnly = DriverCommands.arenaOnly
  val typesOnly = DriverCommands.typesOnly
  val allocMap = DriverCommands.allocMap
  val lowerChecked = DriverCommands.lowerChecked
  val lowerDiagnostics = DriverCommands.lowerDiagnostics
  val lowerForCheck = DriverCommands.lowerForCheck
  val arenaBlocks = DriverCommands.arenaBlocks
  val irOnly = DriverCommands.irOnly
  val llvmOnly = DriverCommands.llvmOnly

  val build = DriverBuild.build
  val runFile = DriverBuild.runFile
  val newCommand = DriverNew.newCommand
  val fmtCommand = DriverFmt.fmtCommand

  val usage = DriverCli.usage
  val versionLine = DriverCli.versionLine
  val parseMemory = DriverCli.parseMemory
  val main = DriverCli.main
end
