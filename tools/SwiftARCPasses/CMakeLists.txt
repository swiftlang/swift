add_llvm_loadable_module(SwiftARCPasses
  SwiftARCPasses.cpp)

target_link_libraries(SwiftARCPasses swiftOptimizeARC)
set_output_directory(SwiftARCPasses ${SWIFT_RUNTIME_OUTPUT_INTDIR} ${SWIFT_LIBRARY_OUTPUT_INTDIR})
