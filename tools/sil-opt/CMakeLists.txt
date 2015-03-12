add_swift_executable(sil-opt
  SILOpt.cpp
  LINK_LIBRARIES
    swiftFrontend
    swiftIRGen
    swiftSILGen
    swiftSILPasses
  COMPONENT_DEPENDS
    option bitreader bitwriter ipo objcarcopts support ${LLVM_TARGETS_TO_BUILD})

swift_install_in_component(tools
    TARGETS sil-opt
    RUNTIME DESTINATION bin)

