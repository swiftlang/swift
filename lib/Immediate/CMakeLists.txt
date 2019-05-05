add_swift_host_library(swiftImmediate STATIC
  Immediate.cpp
  REPL.cpp
  LLVM_LINK_COMPONENTS
    executionengine
    linker
    mcjit
    transformutils)
target_link_libraries(swiftImmediate PRIVATE
  swiftFrontend
  swiftIDE
  swiftIRGen
  swiftSILGen
  swiftSILOptimizer)

if(HAVE_UNICODE_LIBEDIT)
  target_link_libraries(swiftImmediate PRIVATE edit)
endif()
