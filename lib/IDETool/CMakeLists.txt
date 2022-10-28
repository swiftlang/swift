
add_swift_host_library(swiftIDETool STATIC
  CompileInstance.cpp
  CompletionInstance.cpp
  DependencyChecking.cpp
  )

target_link_libraries(swiftIDETool PRIVATE
  swiftAST
  swiftDriver
  swiftFrontend
  swiftFrontendTool)

set_swift_llvm_is_available(swiftIDETool)
