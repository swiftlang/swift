add_swift_library(swiftFrontendTool
  FrontendTool.cpp
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
)
