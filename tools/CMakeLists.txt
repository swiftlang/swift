include(CheckIncludeFiles)
check_include_files("xpc/xpc.h" HAVE_XPC_H)

if(HAVE_XPC_H AND SWIFT_BUILD_SOURCEKIT)
  set(BUILD_SOURCEKIT_XPC_SERVICE_default TRUE)
else()
  set(BUILD_SOURCEKIT_XPC_SERVICE_default FALSE)
endif()

option(BUILD_SOURCEKIT_XPC_SERVICE
  "Whether or not the SourceKit XPC service should be built"
  ${BUILD_SOURCEKIT_XPC_SERVICE_default})

# Add generated SourceKit UID header to global dependencies.
if(SWIFT_BUILD_SOURCEKIT)
  list(APPEND LLVM_COMMON_DEPENDS generated_sourcekit_uids)
endif()

add_swift_tool_subdirectory(driver)
add_swift_tool_subdirectory(swift-compatibility-symbols)
add_swift_tool_subdirectory(swift-demangle)
add_swift_tool_subdirectory(swift-demangle-yamldump)
add_swift_tool_subdirectory(swift-def-to-strings-converter)
add_swift_tool_subdirectory(swift-serialize-diagnostics)
add_swift_tool_subdirectory(swift-ast-script)
add_swift_tool_subdirectory(swift-refactor)
add_swift_tool_subdirectory(libSwiftScan)
add_swift_tool_subdirectory(libStaticMirror)
add_swift_tool_subdirectory(libMockPlugin)
add_swift_tool_subdirectory(swift-plugin-server)

if(SWIFT_INCLUDE_TESTS OR SWIFT_INCLUDE_TEST_BINARIES)
  add_swift_tool_subdirectory(swift-ide-test)
  add_swift_tool_subdirectory(swift-remoteast-test)
  add_swift_tool_subdirectory(lldb-moduleimport-test)
endif()

if(LLVM_USE_SANITIZE_COVERAGE)
add_swift_tool_subdirectory(swift-demangle-fuzzer)
add_swift_tool_subdirectory(swift-reflection-fuzzer)
endif()

if(SWIFT_BUILD_SOURCEKIT)
  add_swift_tool_subdirectory(SourceKit)
endif()

if(SWIFT_HOST_VARIANT STREQUAL "macosx")
  # Only build Darwin-specific tools when deploying to OS X.
  add_swift_tool_subdirectory(swift-stdlib-tool)
endif()

add_swift_tool_subdirectory(swift-reflection-dump)
