add_swift_library(swiftPrintAsObjC
  PrintAsObjC.cpp

  LINK_LIBRARIES
    swiftIDE
    swiftFrontend
    swiftClangImporter
    swiftAST)

