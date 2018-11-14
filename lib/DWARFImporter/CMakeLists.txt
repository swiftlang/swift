add_swift_host_library(swiftDWARFImporter STATIC
  DWARFImporter.cpp
  LINK_LIBRARIES
    swiftAST
    swiftParse
)

#add_dependencies(swiftDWARFImporter )
