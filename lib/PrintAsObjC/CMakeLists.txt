add_swift_host_library(swiftPrintAsObjC STATIC
  PrintAsObjC.cpp
  LINK_LIBRARIES
    swiftIDE
    swiftFrontend
    swiftClangImporter
    swiftAST)

