add_swift_executable(swift-ide-test
  swift-ide-test.cpp
  ModuleAPIDiff.cpp
  XMLValidator.cpp
  LINK_LIBRARIES
    swiftDriver
    swiftFrontend
    swiftIDE
)

# If libxml2 is available, make it available for swift-ide-test.
if(SWIFT_HAVE_LIBXML)
  include_directories(SYSTEM ${LIBXML2_INCLUDE_DIR})
  target_link_libraries(swift-ide-test ${LIBXML2_LIBRARIES})
  add_definitions(-DSWIFT_HAVE_LIBXML="1")
endif()

swift_install_in_component(tools
    TARGETS swift-ide-test
    RUNTIME DESTINATION bin)


# Create a symlink for swift-api-dump.py in the bin directory
add_custom_command(TARGET swift-ide-test POST_BUILD
    COMMAND
      "${CMAKE_COMMAND}" "-E" "create_symlink"
      "${SWIFT_SOURCE_DIR}/utils/swift-api-dump.py"
      "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-api-dump.py"
    COMMENT "Creating development symlink for swift-api-dump.py")
