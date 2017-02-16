add_swift_library(swiftFrontendTool STATIC
  FrontendTool.cpp
  ReferenceDependencies.cpp
  DEPENDS SwiftOptions
  LINK_LIBRARIES
    swiftIDE
    swiftIRGen swiftSIL swiftSILGen swiftSILOptimizer
    swiftImmediate
    swiftSerialization
    swiftPrintAsObjC
    swiftFrontend
    swiftClangImporter
    swiftOption
    clangAPINotes
    clangBasic
)
