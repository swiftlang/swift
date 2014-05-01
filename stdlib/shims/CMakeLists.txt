set(sources shims.h HeapObject.h RefCount.h module.map)
set(output_dir "${SWIFTLIB_DIR}/shims")

set(commands)
set(outputs)
foreach(input ${sources})
  list(APPEND outputs "${output_dir}/${input}")
  list(APPEND commands
    COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${input}"
    "${output_dir}/${input}")
endforeach()

add_custom_command(OUTPUT "${output_dir}" ${outputs}
  COMMAND ${CMAKE_COMMAND} -E make_directory "${output_dir}"
  ${commands}
  DEPENDS ${sources}
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  COMMENT "Copying SwiftShims module to ${output_dir}"
  )

add_custom_target(
  copy_shim_headers
  DEPENDS "${output_dir}" ${outputs}
  SOURCES ${sources}
  )

# Symlink in the Clang headers.
add_custom_command(TARGET copy_shim_headers POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E create_symlink
    "${SWIFT_PATH_TO_CLANG_BUILD}/${CMAKE_CFG_INTDIR}/lib/clang"
    "${SWIFTLIB_DIR}/clang"
  COMMENT "Symlinking Clang resource headers into ${SWIFTLIB_DIR}/clang")

# Symlink in arclite from the XcodeDefault toolchain.
execute_process(COMMAND xcrun --toolchain XcodeDefault -find clang 
  OUTPUT_VARIABLE toolchain_dir 
  OUTPUT_STRIP_TRAILING_WHITESPACE)
string(LENGTH "${toolchain_dir}" len)
math(EXPR len "${len} - 14")  # "/usr/bin/clang"
string(SUBSTRING "${toolchain_dir}" 0 ${len} toolchain_dir)

add_custom_command(TARGET copy_shim_headers POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E create_symlink
    "${toolchain_dir}/usr/lib/arc"
    "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib/arc"
  COMMENT "Symlinking toolchain's arclite into ${SWIFTLIB_DIR}/arc")

install(FILES ${sources} DESTINATION "lib/swift/shims")

# Install Clang headers under the Swift library so that an installed swift's
# module importer can find the compiler headers corresponding to its clang.
install(
  DIRECTORY "${SWIFT_PATH_TO_CLANG_BUILD}/lib/clang"
  DESTINATION "lib/swift"
  PATTERN "*.h")

