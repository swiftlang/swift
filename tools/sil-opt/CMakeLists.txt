add_swift_executable(sil-opt
  SILOpt.cpp
  LINK_LIBRARIES
    swiftFrontend
    swiftIRGen
    swiftSILGen
    swiftSILOptimizer
)

swift_install_in_component(tools
    TARGETS sil-opt
    RUNTIME DESTINATION bin)

