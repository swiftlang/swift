add_swift_host_library(swiftFrontendTool STATIC
  FrontendTool.cpp
  ImportedModules.cpp
  ReferenceDependencies.cpp
  TBD.cpp
  DEPENDS
    swift-syntax-generated-headers SwiftOptions
  INTERFACE_LINK_LIBRARIES
    clangAPINotes
    clangBasic
)
target_link_libraries(swiftFrontendTool PRIVATE
    swiftClangImporter
    swiftDemangling
    swiftDWARFImporter
    swiftFrontend
    swiftIDE
    swiftImmediate
    swiftIndex
    swiftIRGen
    swiftOption
    swiftPrintAsObjC
    swiftSerialization
    swiftSIL
    swiftSILGen
    swiftSILOptimizer
    swiftTBDGen)
