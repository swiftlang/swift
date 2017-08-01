add_swift_library(swiftPrintAsObjC STATIC
  PrintAsObjC.cpp
  DEPENDS
    swift-syntax-generated-headers
  LINK_LIBRARIES
    swiftIDE
    swiftFrontend
    swiftClangImporter
    swiftAST)

