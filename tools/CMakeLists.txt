include(CheckIncludeFiles)
check_include_files("xpc/xpc.h" HAVE_XPC_H)

swift_is_installing_component(sourcekit-inproc SOURCEKIT_INSTALLING_INPROC)

if(HAVE_XPC_H AND SWIFT_BUILD_SOURCEKIT AND NOT SOURCEKIT_INSTALLING_INPROC)
  set(BUILD_SOURCEKIT_XPC_SERVICE TRUE)
else()
  set(BUILD_SOURCEKIT_XPC_SERVICE FALSE)
endif()

# Add generated libSyntax headers to global dependencies.
list(APPEND LLVM_COMMON_DEPENDS swift-syntax-generated-headers)
if(SWIFT_BUILD_SOURCEKIT)
  list(APPEND LLVM_COMMON_DEPENDS generated_sourcekit_uids)
endif()

add_swift_tool_subdirectory(driver)
add_swift_tool_subdirectory(sil-opt)
add_swift_tool_subdirectory(swift-ide-test)
add_swift_tool_subdirectory(swift-remoteast-test)
add_swift_tool_subdirectory(swift-demangle)
add_swift_tool_subdirectory(swift-demangle-yamldump)
add_swift_tool_subdirectory(lldb-moduleimport-test)
add_swift_tool_subdirectory(sil-func-extractor)
add_swift_tool_subdirectory(sil-llvm-gen)
add_swift_tool_subdirectory(sil-nm)
add_swift_tool_subdirectory(sil-passpipeline-dumper)
add_swift_tool_subdirectory(swift-llvm-opt)
add_swift_tool_subdirectory(swift-api-digester)
add_swift_tool_subdirectory(swift-syntax-test)
add_swift_tool_subdirectory(swift-refactor)

if(LLVM_USE_SANITIZE_COVERAGE)
add_swift_tool_subdirectory(swift-demangle-fuzzer)
endif()

if(SWIFT_BUILD_STDLIB AND SWIFT_BUILD_SDK_OVERLAY)
  set(BUILD_FOUNDATION TRUE)
else()
  set(BUILD_FOUNDATION FALSE)
endif()

if(SWIFT_BUILD_SOURCEKIT)
  add_swift_tool_subdirectory(SourceKit)
endif()

if(SWIFT_HOST_VARIANT STREQUAL "macosx")
  # Only build Darwin-specific tools when deploying to OS X.
  add_swift_tool_subdirectory(swift-stdlib-tool)
endif()

add_swift_tool_subdirectory(swift-reflection-dump)
