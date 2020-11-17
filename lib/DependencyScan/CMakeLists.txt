set_swift_llvm_is_available()
add_swift_host_library(SwiftScan STATIC
  DependencyScanningTool.cpp
  ScanDependencies.cpp)
add_dependencies(SwiftScan
  swift-syntax-generated-headers
  SwiftOptions)

target_link_libraries(SwiftScan INTERFACE
  clangAPINotes
  clangBasic)

target_link_libraries(SwiftScan PRIVATE
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
