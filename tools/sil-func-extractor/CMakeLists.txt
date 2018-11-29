add_swift_host_tool(sil-func-extractor
  SILFunctionExtractor.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(sil-func-extractor
                      PRIVATE
                        swiftClangImporter
                        swiftFrontend
                        swiftSerialization
                        swiftSILGen
                        swiftSILOptimizer)
