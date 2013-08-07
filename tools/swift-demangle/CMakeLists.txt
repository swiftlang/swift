add_swift_executable(swift-demangle
  swift-demangle.cpp
  DEPENDS swiftBasic
  COMPONENT_DEPENDS support ${LLVM_TARGETS_TO_BUILD})

target_link_libraries(swift-demangle curses)

install(TARGETS swift-demangle
  RUNTIME DESTINATION bin)
