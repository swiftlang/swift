add_swift_host_tool(sil-opt
  SILOpt.cpp
  LLVM_COMPONENT_DEPENDS
    DebugInfoCodeView
  SWIFT_COMPONENT tools
)
target_link_libraries(sil-opt
                      PRIVATE
                        swiftFrontend
                        swiftIRGen
                        swiftSILGen
                        swiftSILOptimizer
                        # Clang libraries included to appease the linker on linux.
                        clangBasic
                        clangCodeGen)

