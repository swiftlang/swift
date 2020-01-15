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
if(LibEdit_FOUND AND LibEdit_HAS_UNICODE)
  target_compile_definitions(swiftImmediate PRIVATE
    HAVE_LIBEDIT)
  target_link_libraries(swiftImmediate PRIVATE
    ${LibEdit_LIBRARIES})
endif()
