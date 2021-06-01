

add_swift_host_library(swiftDependencyScan STATIC
  DependencyScanningTool.cpp
  ModuleDependencyCacheSerialization.cpp
  ScanDependencies.cpp
  StringUtils.cpp)

target_link_libraries(swiftDependencyScan INTERFACE
  clangBasic)

target_link_libraries(swiftDependencyScan PRIVATE
    swiftClangImporter
    swiftFrontend)
