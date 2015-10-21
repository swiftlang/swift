add_swift_executable(swift-llvm-opt
  LLVMOpt.cpp
  LINK_LIBRARIES
  swiftIRGen
)

swift_install_in_component(tools
    TARGETS swift-llvm-opt
    RUNTIME DESTINATION bin)

