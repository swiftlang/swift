add_swift_host_tool(swift-demangle-yamldump
  swift-demangle-yamldump.cpp
  LLVM_LINK_COMPONENTS support
  SWIFT_COMPONENT testsuite-tools
  )
target_link_libraries(swift-demangle-yamldump
                      PRIVATE
                        swiftDemangling)
