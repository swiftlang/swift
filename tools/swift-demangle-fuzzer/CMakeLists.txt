add_swift_host_tool(swift-demangle-fuzzer
  swift-demangle-fuzzer.cpp
  LLVM_COMPONENT_DEPENDS support
  SWIFT_COMPONENT compiler
  )
target_link_libraries(swift-demangle-fuzzer
                      PRIVATE
                        swiftDemangling)
target_compile_options(swift-demangle-fuzzer PRIVATE "-fsanitize=fuzzer")
target_link_libraries(swift-demangle-fuzzer PRIVATE "-fsanitize=fuzzer")
