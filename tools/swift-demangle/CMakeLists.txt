add_swift_host_tool(swift-demangle
  swift-demangle.cpp
  LLVM_LINK_COMPONENTS support
  SWIFT_COMPONENT compiler
  )
target_link_libraries(swift-demangle
                      PRIVATE
                        swiftDemangling)
