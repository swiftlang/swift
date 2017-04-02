add_swift_library(swiftTBDGen STATIC
  TBDGen.cpp
  LINK_LIBRARIES
    swiftAST
    swiftSIL
)
