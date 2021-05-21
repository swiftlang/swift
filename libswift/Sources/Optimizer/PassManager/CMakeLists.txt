
add_swift_host_library(LibSwiftOptimizer
  STATIC
  IGNORE_LLVM_UPDATE_COMPILE_FLAGS
  PassRegistration.swift)

target_link_libraries(LibSwiftOptimizer PRIVATE LibSwiftSIL)
