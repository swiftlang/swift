
add_swift_host_library(swiftFrontendTool STATIC
  FrontendTool.cpp
  ImportedModules.cpp
  LoadedModuleTrace.cpp
  TBD.cpp)
add_dependencies(swiftFrontendTool
  SwiftOptions)
target_link_libraries(swiftFrontendTool INTERFACE
  clangAPINotes
  clangBasic)
target_link_libraries(swiftFrontendTool PRIVATE
    swiftClangImporter
    swiftConstExtract
    swiftDemangling
    swiftDependencyScan
    swiftFrontend
    swiftImmediate
    swiftIndex
    swiftIRGen
    swiftMigrator
    swiftOption
    swiftPrintAsClang
    swiftSerialization
    swiftSIL
    swiftSILGen
    swiftSILOptimizer
    swiftThreading)

set_swift_llvm_is_available(swiftFrontendTool)
