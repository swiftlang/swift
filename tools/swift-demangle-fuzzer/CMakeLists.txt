set(LLVM_LINK_COMPONENTS Support)
add_swift_fuzzer_host_tool(swift-demangle-fuzzer
  swift-demangle-fuzzer.cpp
  SWIFT_COMPONENT compiler
  )
target_link_libraries(swift-demangle-fuzzer
                      PRIVATE
                        swiftDemangling)
