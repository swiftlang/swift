add_swift_tool_subdirectory(driver)
add_swift_tool_subdirectory(sil-opt)
add_swift_tool_subdirectory(swift-ide-test)
add_swift_tool_subdirectory(swift-remoteast-test)
add_swift_tool_subdirectory(swift-demangle)
add_swift_tool_subdirectory(lldb-moduleimport-test)
add_swift_tool_subdirectory(sil-extract)
add_swift_tool_subdirectory(swift-llvm-opt)

if(SWIFT_HOST_VARIANT STREQUAL "macosx")
  # Only build Darwin-specific tools when deploying to OS X.
  add_swift_tool_subdirectory(swift-api-digester)
endif()

if(SWIFT_BUILD_SOURCEKIT)
  add_swift_tool_subdirectory(SourceKit)
endif()


is_sdk_requested("${SWIFT_HOST_VARIANT_SDK}" SWIFT_HOST_SDK_REQUESTED)
if(SWIFT_BUILD_STDLIB AND SWIFT_HOST_SDK_REQUESTED)
  add_swift_tool_subdirectory(swift-reflection-dump)
endif()
