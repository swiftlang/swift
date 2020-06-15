add_swift_host_tool(swift-ide-test
  swift-ide-test.cpp
  ModuleAPIDiff.cpp
  XMLValidator.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(swift-ide-test
                      PRIVATE
                        swiftAST
                        swiftDriver
                        swiftFrontend
                        swiftIDE)

# If libxml2 is available, make it available for swift-ide-test.
if(LLVM_ENABLE_LIBXML2)
  include_directories(SYSTEM ${LIBXML2_INCLUDE_DIR})
  target_link_libraries(swift-ide-test PRIVATE ${LIBXML2_LIBRARIES})
  target_compile_definitions(swift-ide-test PRIVATE SWIFT_HAVE_LIBXML=1)
  if("${CMAKE_HOST_SYSTEM_NAME}" STREQUAL "OpenBSD" AND NOT CMAKE_CROSSCOMPILING)
    include_directories(SYSTEM "/usr/local/include")
  endif()
endif()

# Create a symlink for swift-api-dump.py in the bin directory
swift_create_post_build_symlink(swift-ide-test
  SOURCE "${SWIFT_SOURCE_DIR}/utils/swift-api-dump.py"
  DESTINATION "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-api-dump.py"
  COMMENT "Creating development symlink for swift-api-dump.py.")

