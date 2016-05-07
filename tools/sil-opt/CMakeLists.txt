add_swift_executable(sil-opt
  SILOpt.cpp
  LINK_LIBRARIES
    swiftFrontend
    swiftIRGen
    swiftSILGen
    swiftSILOptimizer
    LLVMDebugInfoCodeView
)

swift_install_in_component(tools
    TARGETS sil-opt
    RUNTIME DESTINATION bin)

