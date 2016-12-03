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

target_link_libraries(swift edit)

add_llvm_tool_symlink(swiftc swift ALWAYS_GENERATE)
add_llvm_tool_symlink(swift-autolink-extract swift ALWAYS_GENERATE)
add_llvm_tool_symlink(swift-format swift ALWAYS_GENERATE)

# If building as part of clang, make sure the headers are installed.
if(NOT SWIFT_BUILT_STANDALONE)
  add_dependencies(swift clang-headers)
endif()

# Platforms that have a REPL need extra libraries to be linked into the 'swift'
# binary.
include(SwiftDarwin)
if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
  # Link in CoreFoundation on Darwin.
  find_library(CORE_FOUNDATION NAMES CoreFoundation)
  target_link_libraries(swift ${CORE_FOUNDATION})

  # Link in arclite on Darwin.
  get_default_toolchain_dir(toolchain_dir)
  set(SWIFT_REPL_ARCLITE "${toolchain_dir}/usr/lib/arc/libarclite_${SWIFT_HOST_VARIANT}.a")
  set_property(TARGET swift APPEND_STRING PROPERTY
      LINK_FLAGS " -lobjc -Wl,-force_load,\"${SWIFT_REPL_ARCLITE}\"")
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
