add_swift_host_library(swiftFrontendTool STATIC
  FrontendTool.cpp
  ImportedModules.cpp
  ReferenceDependencies.cpp
  TBD.cpp
  DEPENDS
    swift-syntax-generated-headers SwiftOptions
  LINK_LIBRARIES
    swiftIndex
    swiftIDE
    swiftTBDGen swiftIRGen swiftSIL swiftSILGen swiftSILOptimizer
    swiftDemangling
    swiftImmediate
    swiftSerialization
    swiftPrintAsObjC
    swiftFrontend
    swiftClangImporter
    swiftOption
    clangAPINotes
    clangBasic
)
