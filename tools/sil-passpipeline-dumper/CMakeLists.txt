add_swift_host_tool(sil-passpipeline-dumper
  SILPassPipelineDumper.cpp
  LINK_LIBRARIES
    swiftFrontend
    swiftSILGen
    swiftSILOptimizer
    swiftSerialization
    swiftClangImporter
    # FIXME: Circular dependencies require re-listing these libraries.
    swiftSema
    swiftAST
  LLVM_COMPONENT_DEPENDS
    DebugInfoCodeView
  SWIFT_COMPONENT tools
)
