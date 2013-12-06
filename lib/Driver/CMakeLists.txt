add_swift_library(swiftDriver
  Action.cpp
  Compilation.cpp
  Driver.cpp
  Job.cpp
  Options.cpp
  Tool.cpp
  Tools.cpp
  ToolChain.cpp
  ToolChains.cpp
  Types.cpp
  DEPENDS swiftAST swiftBasic)

add_dependencies(swiftDriver SwiftDriverOptions)
