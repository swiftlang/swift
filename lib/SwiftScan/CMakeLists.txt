set_swift_llvm_is_available()
add_swift_host_library(SwiftScanTool STATIC
  DependencyScanningTool.cpp
  ScanDependencies.cpp)
add_dependencies(SwiftScanTool
  swift-syntax-generated-headers
  SwiftOptions)

target_link_libraries(SwiftScanTool INTERFACE
  clangAPINotes
  clangBasic)

target_link_libraries(SwiftScanTool PRIVATE
    swiftClangImporter
    swiftDemangling
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
