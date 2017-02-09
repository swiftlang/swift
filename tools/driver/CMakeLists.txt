add_swift_host_tool(swift
  api_notes.cpp
  driver.cpp
  autolink_extract_main.cpp
  modulewrap_main.cpp
  swift_format_main.cpp
  LINK_LIBRARIES
    swiftDriver
    swiftFrontendTool
  LLVM_COMPONENT_DEPENDS
    DebugInfoCodeView
  SWIFT_COMPONENT compiler
)

if(HAVE_UNICODE_LIBEDIT)
  target_link_libraries(swift edit)
endif()

swift_create_post_build_symlink(swift
  SOURCE "swift${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION "swiftc${CMAKE_EXECUTABLE_SUFFIX}"
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

swift_create_post_build_symlink(swift
  SOURCE "swift${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION "swift-format${CMAKE_EXECUTABLE_SUFFIX}"
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

swift_create_post_build_symlink(swift
  SOURCE "swift${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION "swift-autolink-extract${CMAKE_EXECUTABLE_SUFFIX}"
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

# If building as part of clang, make sure the headers are installed.
if(NOT SWIFT_BUILT_STANDALONE)
  add_dependencies(swift clang-headers)
endif()

swift_install_in_component(compiler
    FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swiftc"
    DESTINATION "bin")
swift_install_in_component(autolink-driver
    FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-autolink-extract"
    DESTINATION "bin")
swift_install_in_component(editor-integration
    FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-format"
    DESTINATION "bin")
