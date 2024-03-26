

set(swiftDriver_sources
  Action.cpp
  Compilation.cpp
  DarwinToolChains.cpp
  Driver.cpp
  FrontendUtil.cpp
  Job.cpp
  PrettyStackTrace.cpp
  ToolChain.cpp
  ToolChains.cpp
  UnixToolChains.cpp
  WebAssemblyToolChains.cpp
  WindowsToolChains.cpp
  )

set(swiftDriver_targetDefines)

add_swift_host_library(swiftDriver STATIC
  ${swiftDriver_sources})
add_dependencies(swiftDriver
  SwiftOptions)
target_link_libraries(swiftDriver PRIVATE
  swiftAST
  swiftBasic
  swiftOption)

set_swift_llvm_is_available(swiftDriver)
