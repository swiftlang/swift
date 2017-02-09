add_swift_library(swiftFrontendTool STATIC
  FrontendTool.cpp
  ReferenceDependencies.cpp
  DEPENDS SwiftOptions
  LINK_LIBS
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
