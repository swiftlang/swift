add_swift_host_tool(sil-func-extractor
  SILFunctionExtractor.cpp
  LINK_LIBRARIES
    swiftFrontend
    swiftSILGen
    swiftSILOptimizer
    swiftSerialization
    swiftClangImporter
  LLVM_COMPONENT_DEPENDS
    DebugInfoCodeView
  SWIFT_COMPONENT tools
)
