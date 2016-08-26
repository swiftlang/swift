add_swift_executable(sil-opt
  SILOpt.cpp
  LINK_LIBRARIES
    swiftFrontend
    swiftIRGen
    swiftSILGen
    swiftSILOptimizer
  LLVM_COMPONENT_DEPENDS
    DebugInfoCodeView
)

swift_install_in_component(tools
    TARGETS sil-opt
    RUNTIME DESTINATION bin)

