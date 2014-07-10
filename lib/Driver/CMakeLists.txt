add_swift_library(swiftDriver
  Action.cpp
  Compilation.cpp
  Driver.cpp
  Job.cpp
  OutputFileMap.cpp
  Tool.cpp
  Tools.cpp
  ToolChain.cpp
  ToolChains.cpp
  Types.cpp
  Util.cpp
  DEPENDS swiftAST swiftBasic swiftOption)
