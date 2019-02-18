add_swift_host_tool(sil-llvm-gen
  SILLLVMGen.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(sil-llvm-gen
                      PRIVATE
                        swiftFrontend
                        swiftIRGen
                        swiftSILGen
                        swiftSILOptimizer
                        # Clang libraries included to appease the linker on linux.
                        clangBasic
                        clangCodeGen)
