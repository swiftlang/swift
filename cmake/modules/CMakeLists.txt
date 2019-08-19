set(SWIFT_INSTALL_PACKAGE_DIR lib${LLVM_LIBDIR_SUFFIX}/cmake/swift)
set(swift_cmake_builddir "${SWIFT_BINARY_DIR}/${SWIFT_INSTALL_PACKAGE_DIR}")

# Exports for those who want to build against an installed Swift
set(SWIFT_EXPORTS_FILE "${swift_cmake_builddir}/SwiftExports.cmake")
get_property(SWIFT_EXPORTS GLOBAL PROPERTY SWIFT_EXPORTS)
export(TARGETS ${SWIFT_EXPORTS} FILE ${SWIFT_EXPORTS_FILE})

# Export targets that are only available when building against a Swift build
# tree
set(SWIFT_BUILDTREE_EXPORTS_FILE "${swift_cmake_builddir}/SwiftBuildTreeExports.cmake")
get_property(SWIFT_BUILDTREE_EXPORTS GLOBAL PROPERTY SWIFT_BUILDTREE_EXPORTS)
export(TARGETS ${SWIFT_BUILDTREE_EXPORTS} FILE ${SWIFT_BUILDTREE_EXPORTS_FILE})

# Generate SwiftConfig.cmake for the build tree
set(SWIFT_CONFIG_EXPORTS "${SWIFT_EXPORTS};${SWIFT_BUILDTREE_EXPORTS}")
set(SWIFT_CONFIG_EXPORTS_FILE "${SWIFT_EXPORTS_FILE}")
set(SWIFT_CONFIG_BINARY_DIR "${SWIFT_BINARY_DIR}")
set(SWIFT_CONFIG_INCLUDE_DIRS
  "${SWIFT_INCLUDE_DIR}"
  "${SWIFT_MAIN_INCLUDE_DIR}"
)
set(SWIFT_CONFIG_INCLUDE_DIR "${SWIFT_INCLUDE_DIR}")
set(SWIFT_CONFIG_LIBRARY_DIRS "${SWIFT_LIBRARY_OUTPUT_INTDIR}")
set(SWIFT_CONFIG_CMAKE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
set(swift_config_include_buildtree_exports
"include(\"${SWIFT_BUILDTREE_EXPORTS_FILE}\")")

configure_file(
  SwiftConfig.cmake.in
  ${swift_cmake_builddir}/SwiftConfig.cmake
  @ONLY)
set(swift_config_include_buildtree_exports)

# Generate SwiftConfig.cmake for the install tree
set(SWIFT_CONFIG_CODE
"# Compute the installation prefix from this SwiftConfig.cmake location.
get_filename_component(SWIFT_INSTALL_PREFIX \"\${CMAKE_CURRENT_LIST_FILE}\" PATH)")
# Construct the proper number of get_filename_component(... PATH)
# calls to compute the installation prefix.
file(TO_CMAKE_PATH "${SWIFT_INSTALL_PACKAGE_DIR}" SWIFT_INSTALL_PACKAGE_DIR)
string(REGEX REPLACE "/" ";" _count "${SWIFT_INSTALL_PACKAGE_DIR}")
foreach(p ${_count})
  set(SWIFT_CONFIG_CODE "${SWIFT_CONFIG_CODE}
get_filename_component(SWIFT_INSTALL_PREFIX \"\${SWIFT_INSTALL_PREFIX}\" PATH)")
endforeach(p)
set(SWIFT_CONFIG_BINARY_DIR "\${SWIFT_INSTALL_PREFIX}")
set(SWIFT_CONFIG_CMAKE_DIR "\${SWIFT_INSTALL_PREFIX}/${SWIFT_INSTALL_PACKAGE_DIR}")
set(SWIFT_CONFIG_EXPORTS "${SWIFT_EXPORTS}")
set(SWIFT_CONFIG_EXPORTS_FILE "\${SWIFT_CMAKE_DIR}/SwiftExports.cmake")
set(SWIFT_CONFIG_INCLUDE_DIRS "\${SWIFT_INSTALL_PREFIX}/include")
set(SWIFT_CONFIG_LIBRARY_DIRS "\${SWIFT_INSTALL_PREFIX}/lib\${LLVM_LIBDIR_SUFFIX}")
configure_file(
  SwiftConfig.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/SwiftConfig.cmake
  @ONLY)

if (NOT LLVM_INSTALL_TOOLCHAIN_ONLY)
  get_property(swift_has_exports GLOBAL PROPERTY SWIFT_HAS_EXPORTS)
  if(swift_has_exports)
    install(EXPORT SwiftExports DESTINATION ${SWIFT_INSTALL_PACKAGE_DIR})
  endif()
  install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/SwiftConfig.cmake
    DESTINATION ${SWIFT_INSTALL_PACKAGE_DIR})
endif()
