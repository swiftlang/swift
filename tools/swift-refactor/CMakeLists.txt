add_swift_host_tool(swift-refactor
  swift-refactor.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(swift-refactor
                      PRIVATE
                        swiftDriver
                        swiftFrontend
                        swiftIDE)
if (NOT LLVM_ENABLE_ASSERTIONS)
  target_link_options(
    swift-refactor
    PRIVATE
    "LINKER:-weak_library"
    "LINKER:$<TARGET_FILE:swiftExternalSILVerifier>")
endif()
