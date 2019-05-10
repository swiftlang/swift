add_swift_host_library(swiftDWARFImporter STATIC
  DWARFImporter.cpp)
target_link_libraries(swiftDWARFImporter PRIVATE
    swiftAST
    swiftParse
)
