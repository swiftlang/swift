add_swift_target_executable(swift-remoteast-test
  swift-remoteast-test.cpp
  LINK_LIBRARIES
    swiftRemoteAST
    LLVMCoverage)

