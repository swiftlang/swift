add_swift_host_tool(swift-frontend
  driver.cpp
  autolink_extract_main.cpp
  modulewrap_main.cpp
  swift_indent_main.cpp
  swift_symbolgraph_extract_main.cpp
  SWIFT_COMPONENT compiler
)
target_link_libraries(swift-frontend
                      PRIVATE
                        swiftDriver
                        swiftFrontendTool
                        swiftSymbolGraphGen
                        LLVMBitstreamReader)

# Generate and install supported feature JSON file to the toolchain.
if(NOT CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
  set(DUMMY_SWIFT_INPUT "${SWIFT_RUNTIME_OUTPUT_INTDIR}/dummyInput.swift")
  set(INT_FEATURE_FILE "${SWIFT_RUNTIME_OUTPUT_INTDIR}/SupportedFeatures.json")
  add_custom_command(TARGET swift-frontend POST_BUILD
                     COMMAND "${CMAKE_COMMAND}" "-E" "touch" "${DUMMY_SWIFT_INPUT}"
                     COMMAND "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-frontend"
                             "-emit-supported-features" "-o"
                             "${INT_FEATURE_FILE}"
                             "${DUMMY_SWIFT_INPUT}")
  swift_install_in_component(FILES "${INT_FEATURE_FILE}"
                             DESTINATION "bin"
                             COMPONENT compiler)
endif()

swift_create_post_build_symlink(swift-frontend
  SOURCE "swift-frontend${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION "swift${CMAKE_EXECUTABLE_SUFFIX}"
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

swift_create_post_build_symlink(swift-frontend
  SOURCE "swift-frontend${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION "swiftc${CMAKE_EXECUTABLE_SUFFIX}"
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

swift_create_post_build_symlink(swift-frontend
  SOURCE "swift-frontend${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION "swift-indent${CMAKE_EXECUTABLE_SUFFIX}"
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

swift_create_post_build_symlink(swift-frontend
  SOURCE "swift-frontend${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION "swift-symbolgraph-extract${CMAKE_EXECUTABLE_SUFFIX}"
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

swift_create_post_build_symlink(swift-frontend
  SOURCE "swift-frontend${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION "swift-autolink-extract${CMAKE_EXECUTABLE_SUFFIX}"
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

add_swift_tool_symlink(swift swift-frontend compiler)
add_swift_tool_symlink(swiftc swift-frontend compiler)
add_swift_tool_symlink(swift-symbolgraph-extract swift-frontend compiler)
add_swift_tool_symlink(swift-autolink-extract swift-frontend autolink-driver)
add_swift_tool_symlink(swift-indent swift-frontend editor-integration)

# If building as part of clang, make sure the headers are installed.
if(NOT SWIFT_BUILT_STANDALONE)
  add_dependencies(swift-frontend clang-resource-headers)
endif()

add_dependencies(compiler swift-frontend)
swift_install_in_component(FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift${CMAKE_EXECUTABLE_SUFFIX}"
                           DESTINATION "bin"
                           COMPONENT compiler)
swift_install_in_component(FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swiftc${CMAKE_EXECUTABLE_SUFFIX}"
                           DESTINATION "bin"
                           COMPONENT compiler)
swift_install_in_component(FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-symbolgraph-extract${CMAKE_EXECUTABLE_SUFFIX}"
                           DESTINATION "bin"
                           COMPONENT compiler)
add_dependencies(autolink-driver swift-frontend)
swift_install_in_component(FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-autolink-extract${CMAKE_EXECUTABLE_SUFFIX}"
                           DESTINATION "bin"
                           COMPONENT autolink-driver)
add_dependencies(editor-integration swift-frontend)
swift_install_in_component(FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-indent${CMAKE_EXECUTABLE_SUFFIX}"
                           DESTINATION "bin"
                           COMPONENT editor-integration)

