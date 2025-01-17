add_swift_host_tool(swift-scan-test
  swift-scan-test.cpp
  LLVM_LINK_COMPONENTS support
  SWIFT_COMPONENT tools
  THINLTO_LD64_ADD_FLTO_CODEGEN_ONLY
  DOES_NOT_USE_SWIFT
)

target_link_libraries(swift-scan-test
                      PRIVATE
                      swiftBasic
                      swiftDependencyScanImpl
                      libSwiftScan)
