#set_swift_llvm_is_available()

add_swift_host_library(swiftDependencyScan STATIC
  DependencyScanningTool.cpp
  ScanDependencies.cpp
  StringUtils.cpp)

target_link_libraries(swiftDependencyScan INTERFACE
  clangBasic)

target_link_libraries(swiftDependencyScan PRIVATE
    swiftClangImporter
    swiftFrontend)
