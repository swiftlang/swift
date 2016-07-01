set(swiftDriver_sources
  Action.cpp
  Compilation.cpp
  DependencyGraph.cpp
  Driver.cpp
  FrontendUtil.cpp
  Job.cpp
  OutputFileMap.cpp
  ParseableOutput.cpp
  ToolChain.cpp
  ToolChains.cpp
  Types.cpp
  )

set(swiftDriver_targetDefines)

add_swift_library(swiftDriver STATIC
  ${swiftDriver_sources}
  DEPENDS SwiftOptions
  LINK_LIBRARIES swiftAST swiftBasic swiftFrontend swiftOption)

