add_swift_host_library(swiftPrintAsObjC STATIC
  PrintAsObjC.cpp)
target_link_libraries(swiftPrintAsObjC PRIVATE
  swiftAST
  swiftClangImporter
  swiftFrontend
  swiftIDE)

