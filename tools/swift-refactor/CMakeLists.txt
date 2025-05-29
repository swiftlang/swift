add_swift_host_tool(swift-refactor
  swift-refactor.cpp
  SWIFT_COMPONENT testsuite-tools
  LLVM_LINK_COMPONENTS support
)
target_link_libraries(swift-refactor
                      PRIVATE
                        swiftDriver
                        swiftFrontend
                        swiftIDE
                        swiftRefactoring)
