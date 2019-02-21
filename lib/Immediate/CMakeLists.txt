add_swift_host_library(swiftImmediate STATIC
  Immediate.cpp
  REPL.cpp
  LINK_LIBRARIES
    swiftIDE
    swiftFrontend
    swiftSILGen
    swiftSILOptimizer
    swiftIRGen
  LLVM_COMPONENT_DEPENDS
    executionengine
    linker
    mcjit
    transformutils)

if(HAVE_UNICODE_LIBEDIT)
  target_link_libraries(swiftImmediate PRIVATE edit)
endif()
