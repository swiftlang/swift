add_swift_host_tool(sil-func-extractor
  SILFunctionExtractor.cpp
  LINK_LIBRARIES
    swiftFrontend
    swiftSILGen
    swiftSILOptimizer
    swiftSerialization
    swiftClangImporter
  SWIFT_COMPONENT tools
)
