add_swift_host_tool(sil-nm
  SILNM.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(sil-nm
                      PRIVATE
                        swiftClangImporter
                        swiftFrontend
                        swiftSerialization
                        swiftSILGen
                        swiftSILOptimizer)
