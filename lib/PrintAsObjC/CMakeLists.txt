add_swift_host_library(swiftPrintAsObjC STATIC
  PrintAsObjC.cpp
  LINK_LIBS
    swiftIDE
    swiftFrontend
    swiftClangImporter
    swiftAST)

