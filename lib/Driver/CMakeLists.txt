add_swift_library(swiftDriver
  Action.cpp
  Compilation.cpp
  Driver.cpp
  FrontendUtil.cpp
  Job.cpp
  OutputFileMap.cpp
  ParseableOutput.cpp
  Tool.cpp
  Tools.cpp
  ToolChain.cpp
  ToolChains.cpp
  Types.cpp
  Util.cpp
  DEPENDS swiftAST swiftBasic swiftFrontend swiftOption)
