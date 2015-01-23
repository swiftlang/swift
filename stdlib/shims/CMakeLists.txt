set(sources
  DarwinShims.h
  FoundationShims.h
  GlobalObjects.h
  HeapObject.h
  RefCount.h
  RuntimeShims.h
  SwiftStddef.h
  SwiftStdint.h
  UnicodeShims.h
  module.map
  )
set(output_dir "${SWIFTLIB_DIR}/shims")

set(commands
    COMMAND ${CMAKE_COMMAND} "-E" "make_directory" "${output_dir}")
set(outputs "${output_dir}")
foreach(input ${sources})
  list(APPEND commands
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
        "${CMAKE_CURRENT_SOURCE_DIR}/${input}"
        "${output_dir}/${input}")
  list(APPEND outputs "${output_dir}/${input}")
endforeach()

add_custom_command_target(unused_var
    ${commands}
    CUSTOM_TARGET_NAME "copy_shim_headers"
    OUTPUT "${outputs}"
    DEPENDS "${sources}"
    COMMENT "Copying SwiftShims module to ${output_dir}")

# Symlink in the Clang headers.
set(clang_headers_locations
    "${LLVM_LIBRARY_DIR}/clang"

    # FIXME: if we want to support separate Clang builds and mix different
    # build configurations of Clang and Swift, this line should be adjusted.
    "${SWIFT_PATH_TO_CLANG_BUILD}/${CMAKE_CFG_INTDIR}/lib/clang")

set(clang_headers_location)
foreach(loc ${clang_headers_locations})
  if("${clang_headers_location}" STREQUAL "" AND EXISTS "${loc}")
    set(clang_headers_location "${loc}")
  endif()
endforeach()
if("${clang_headers_location}" STREQUAL "")
  if(SWIFT_BUILT_STANDALONE)
    message(FATAL_ERROR "Clang headers were not found")
  else()
    set(clang_headers_location "${LLVM_LIBRARY_DIR}/clang")
  endif()
endif()

add_custom_command_target(unused_var
    COMMAND
      "${CMAKE_COMMAND}" "-E" "make_directory" "${SWIFTLIB_DIR}"
    COMMAND
      "${CMAKE_COMMAND}" "-E" "create_symlink"
      "${clang_headers_location}"
      "${SWIFTLIB_DIR}/clang"

    # Create a broken symlink that points to '../clang'.  It is not used when
    # running Swift tools from the build tree directly, but we will install it
    # in such a way that it points to Clang installation.  If the link can be
    # resolved, CMake would try to follow it when installing the project under
    # certain conditions.
    COMMAND
      "${CMAKE_COMMAND}" "-E" "make_directory" "${SWIFTLIB_DIR}/install-tmp/install-tmp"
    COMMAND
      "${CMAKE_COMMAND}" "-E" "create_symlink"
      "../clang"
      "${SWIFTLIB_DIR}/install-tmp/install-tmp/clang"
    CUSTOM_TARGET_NAME "symlink_clang_headers"
    OUTPUT "${SWIFTLIB_DIR}/clang" "${SWIFTLIB_DIR}/install-tmp/install-tmp/clang"
    COMMENT "Symlinking Clang resource headers into ${SWIFTLIB_DIR}/clang")
add_dependencies(copy_shim_headers symlink_clang_headers)

include(SwiftDarwin)
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  # Symlink in arclite from the XcodeDefault toolchain.
  get_default_toolchain_dir(toolchain_dir)
  add_custom_command_target(unused_var
      COMMAND
        "${CMAKE_COMMAND}" "-E" "make_directory"
        "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib"
      COMMAND
        "${CMAKE_COMMAND}" "-E" "create_symlink"
        "${toolchain_dir}/usr/lib/arc"
        "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib/arc"
      CUSTOM_TARGET_NAME "symlink_arclite"
      OUTPUT "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib/arc"
      COMMENT "Symlinking toolchain's arclite into ${SWIFTLIB_DIR}/arc")
  add_dependencies(copy_shim_headers symlink_arclite)

  swift_install_in_component(arclite-symlink
      DIRECTORY "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib/arc"
      DESTINATION "lib")
endif()

swift_install_in_component(compiler
    FILES ${sources}
    DESTINATION "lib/swift/shims")

# Install Clang headers under the Swift library so that an installed Swift's
# module importer can find the compiler headers corresponding to its Clang.
swift_install_in_component(clang-builtin-headers
    DIRECTORY "${SWIFT_PATH_TO_CLANG_BUILD}/lib/clang"
    DESTINATION "lib/swift"
    PATTERN "*.h")

# Alternatively, install a symbolic link to the Clang resource directory.
swift_install_in_component(clang-resource-dir-symlink
    DIRECTORY "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib/swift/install-tmp/install-tmp/clang"
    DESTINATION "lib/swift")

