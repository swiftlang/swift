add_swift_host_tool(sil-func-extractor
  SILFunctionExtractor.cpp
  LLVM_COMPONENT_DEPENDS
    DebugInfoCodeView
  SWIFT_COMPONENT tools
)
target_link_libraries(sil-func-extractor
                      PRIVATE
                        swiftFrontend
                        swiftSILGen
                        swiftSILOptimizer
                        swiftSerialization
                        swiftClangImporter)
