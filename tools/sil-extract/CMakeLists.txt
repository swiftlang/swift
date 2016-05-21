add_swift_executable(sil-extract
  SILExtract.cpp
  LINK_LIBRARIES
    swiftFrontend
    swiftSILGen
    swiftSILOptimizer
    swiftSerialization
    swiftClangImporter
  COMPONENT_DEPENDS
    DebugInfoCodeView
)

swift_install_in_component(tools
    TARGETS sil-extract
    RUNTIME DESTINATION "bin")

