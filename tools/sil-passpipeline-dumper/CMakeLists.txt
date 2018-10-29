add_swift_host_tool(sil-passpipeline-dumper
  SILPassPipelineDumper.cpp
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
