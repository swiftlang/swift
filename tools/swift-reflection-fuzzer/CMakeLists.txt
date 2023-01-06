add_swift_fuzzer_host_tool(swift-reflection-fuzzer
  swift-reflection-fuzzer.cpp
  LLVM_LINK_COMPONENTS support
  SWIFT_COMPONENT testsuite-tools
  )
target_link_libraries(swift-reflection-fuzzer
                      PRIVATE
                        swiftRemoteInspection)
