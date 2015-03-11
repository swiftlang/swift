add_swift_executable(sil-extract
  SILExtract.cpp
  LINK_LIBRARIES
    swiftFrontend
    swiftSILGen
    swiftSILPasses
    swiftSerialization
    swiftClangImporter
  COMPONENT_DEPENDS
    option bitreader bitwriter ipo objcarcopts support ${LLVM_TARGETS_TO_BUILD})

swift_install_in_component(tools
    TARGETS sil-extract
    RUNTIME DESTINATION "bin")

