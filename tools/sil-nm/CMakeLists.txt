add_swift_host_tool(sil-nm
  SILNM.cpp
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
