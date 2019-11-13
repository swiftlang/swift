add_swift_host_tool(swift
  driver.cpp
  autolink_extract_main.cpp
  modulewrap_main.cpp
  swift_indent_main.cpp
  SWIFT_COMPONENT compiler
)
target_link_libraries(swift
                      PRIVATE
                        swiftDriver
                        swiftFrontendTool)
if(HAVE_UNICODE_LIBEDIT)
  target_link_libraries(swift PRIVATE edit)
endif()

swift_create_post_build_symlink(swift
  SOURCE "swift${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION "swiftc${CMAKE_EXECUTABLE_SUFFIX}"
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

swift_create_post_build_symlink(swift
  SOURCE "swift${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION "swift-indent${CMAKE_EXECUTABLE_SUFFIX}"
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

swift_create_post_build_symlink(swift
  SOURCE "swift${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION "swift-autolink-extract${CMAKE_EXECUTABLE_SUFFIX}"
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

add_swift_tool_symlink(swiftc swift compiler)
add_swift_tool_symlink(swift-autolink-extract swift autolink-driver)
add_swift_tool_symlink(swift-indent swift editor-integration)

# If building as part of clang, make sure the headers are installed.
if(NOT SWIFT_BUILT_STANDALONE)
  add_dependencies(swift clang-resource-headers)
endif()

add_dependencies(compiler swift)
swift_install_in_component(FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swiftc${CMAKE_EXECUTABLE_SUFFIX}"
                           DESTINATION "bin"
                           COMPONENT compiler)
add_dependencies(autolink-driver swift)
swift_install_in_component(FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-autolink-extract${CMAKE_EXECUTABLE_SUFFIX}"
                           DESTINATION "bin"
                           COMPONENT autolink-driver)
add_dependencies(editor-integration swift)
swift_install_in_component(FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-indent${CMAKE_EXECUTABLE_SUFFIX}"
                           DESTINATION "bin"
                           COMPONENT editor-integration)
