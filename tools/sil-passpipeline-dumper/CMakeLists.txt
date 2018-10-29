add_swift_host_tool(sil-passpipeline-dumper
  SILPassPipelineDumper.cpp
  LLVM_COMPONENT_DEPENDS
    DebugInfoCodeView
  SWIFT_COMPONENT tools
)
target_link_libraries(sil-passpipeline-dumper
                      PRIVATE
                        swiftFrontend
                        swiftSILGen
                        swiftSILOptimizer
                        swiftSerialization
                        swiftClangImporter
                        # FIXME: Circular dependencies require re-listing these libraries.
                        swiftSema
                        swiftAST)
