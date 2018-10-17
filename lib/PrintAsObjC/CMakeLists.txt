swift_add_library(swiftPrintAsObjC STATIC
  PrintAsObjC.cpp
  LINK_LIBRARIES
    swiftIDE
    swiftFrontend
    swiftClangImporter
    swiftAST)

