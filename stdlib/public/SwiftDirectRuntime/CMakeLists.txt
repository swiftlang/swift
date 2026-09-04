# swiftSwiftDirectRuntime is a static library that provides implementations of the
# fast paths of Swift runtime functions that are suitable for being embedded
# directly into clients.

add_swift_target_library(swiftSwiftDirectRuntime
  STATIC DONT_EMBED_BITCODE NOSWIFTRT
  RetainRelease.s
  C_COMPILE_FLAGS ${SWIFT_RUNTIME_CXX_FLAGS}
  $<$<BOOL:${SWIFT_STDLIB_ENABLE_OBJC_INTEROP}>:-DSWIFT_OBJC_INTEROP=1>
  LINK_FLAGS ${SWIFT_RUNTIME_LINK_FLAGS}
  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}

  DEPLOYMENT_VERSION_OSX ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_OSX}
  DEPLOYMENT_VERSION_IOS ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_IOS}
  DEPLOYMENT_VERSION_TVOS ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_WATCHOS}
  DEPLOYMENT_VERSION_XROS ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_XROS}

  MACCATALYST_BUILD_FLAVOR "zippered"

  INSTALL_IN_COMPONENT compiler
  INSTALL_WITH_SHARED)

# The inline assembly template is read by the compiler at compile time and
# emitted as module-level inline assembly. Copy it to the resource directory
# (lib/swift/ and lib/swift_static/) so the compiler can find it.
set(inline_asm_src "${CMAKE_CURRENT_SOURCE_DIR}/DirectRuntimeRetainRelease-arm64.s")
set(inline_asm_filename "DirectRuntimeRetainRelease-arm64.s")

set(inline_asm_dest "${SWIFTLIB_DIR}/${inline_asm_filename}")
set(inline_asm_static_dest "${SWIFTSTATICLIB_DIR}/${inline_asm_filename}")

add_custom_command(
  OUTPUT "${inline_asm_dest}" "${inline_asm_static_dest}"
  DEPENDS "${inline_asm_src}"
  COMMAND "${CMAKE_COMMAND}" -E copy "${inline_asm_src}" "${inline_asm_dest}"
  COMMAND "${CMAKE_COMMAND}" -E copy "${inline_asm_src}" "${inline_asm_static_dest}")

add_custom_target(copy-direct-retain-release-inline-asm ALL
  DEPENDS "${inline_asm_dest}" "${inline_asm_static_dest}"
  SOURCES "${inline_asm_src}")

swift_install_in_component(
  FILES "${inline_asm_src}"
  DESTINATION "lib/swift/"
  COMPONENT compiler)

if(SWIFT_BUILD_STATIC_STDLIB)
  swift_install_in_component(
    FILES "${inline_asm_src}"
    DESTINATION "lib/swift_static/"
    COMPONENT compiler)
endif()
