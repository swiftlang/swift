# NOTE: This file is responsible for producing the Swift CMake package files.
# Historically it only generated build-tree exports/config. This version:
#   - keeps the build-tree SwiftExports.cmake (useful for in-tree builds)
#   - installs a relocatable SwiftConfig.cmake + SwiftConfigVersion.cmake
#   - generates an install-tree SwiftExports.cmake at install time so that
#     find_package(Swift CONFIG) works from an installed Swift build, without
#     requiring targets to have been installed into a single CMake export set.

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

# Install location for the Swift CMake package (matches existing intent, but
# uses CMAKE_INSTALL_LIBDIR for cross-platform correctness).
set(SWIFT_INSTALL_PACKAGE_DIR "${CMAKE_INSTALL_LIBDIR}/cmake/swift")

# Directory in the build tree where we stage the generated package files.
set(swift_cmake_builddir "${SWIFT_BINARY_DIR}/${SWIFT_INSTALL_PACKAGE_DIR}")

# -----------------------------------------------------------------------------
# Build-tree exports/config (kept for compatibility with existing behavior)
# -----------------------------------------------------------------------------

set(SWIFT_EXPORTS_FILE "${swift_cmake_builddir}/SwiftExports.cmake")
set(SWIFT_EXPORTS_INSTALL_FILE "${swift_cmake_builddir}/SwiftExports-install.cmake")
get_property(SWIFT_EXPORTS GLOBAL PROPERTY SWIFT_EXPORTS)
get_property(SWIFT_BUILDTREE_EXPORTS GLOBAL PROPERTY SWIFT_BUILDTREE_EXPORTS)

# Export both "installing" and "build-tree-only" targets for build-tree usage.
set(SWIFT_CONFIG_EXPORTS ${SWIFT_EXPORTS} ${SWIFT_BUILDTREE_EXPORTS})
if(SWIFT_CONFIG_EXPORTS)
  export(TARGETS ${SWIFT_CONFIG_EXPORTS} FILE "${SWIFT_EXPORTS_FILE}")
endif()

# -----------------------------------------------------------------------------
# Install-tree exports file (generated in build dir, installed as a normal file)
# -----------------------------------------------------------------------------
#
# Avoid install(CODE ...) to generate this file: install-time code does not
# automatically respect DESTDIR staging. By generating it in the build tree and
# installing it via swift_install_in_component(FILES ...), it will be staged
# correctly under DESTDIR.
if(SWIFT_EXPORTS)
  export(TARGETS ${SWIFT_EXPORTS} FILE "${SWIFT_EXPORTS_INSTALL_FILE}")
endif()

# Generate a build-tree SwiftConfig.cmake as well (useful for in-tree consumers).
# Keep existing SWIFT_CONFIG_CODE + version variables, but avoid hardcoding build
# include/lib dirs into the installed package (the installed one is generated
# separately below).
#
# For build-tree config, we preserve the previous values.
set(SWIFT_INCLUDE_DIRS
  ${SWIFT_INCLUDE_DIR}
  ${SWIFT_MAIN_INCLUDE_DIR}
  ${SWIFT_SHIMS_INCLUDE_DIR})
set(SWIFT_LIBRARY_DIRS ${SWIFT_LIBRARY_OUTPUT_INTDIR})

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/SwiftConfig.cmake.in"
  "${swift_cmake_builddir}/SwiftConfig.cmake"
  @ONLY)

# -----------------------------------------------------------------------------
# Install-tree package: SwiftConfig.cmake, SwiftConfigVersion.cmake, SwiftExports.cmake
# -----------------------------------------------------------------------------

# 1) Install-tree SwiftConfig.cmake must be relocatable.
# We generate it into the build staging dir, then install it.
#
# IMPORTANT: This assumes SwiftConfig.cmake.in contains @PACKAGE_INIT@ and uses
# PACKAGE_PREFIX_DIR for installed include/lib dirs, and includes
# "${CMAKE_CURRENT_LIST_DIR}/SwiftExports.cmake".
configure_package_config_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/SwiftConfig.cmake.in"
  "${swift_cmake_builddir}/SwiftConfig.cmake.install"
  INSTALL_DESTINATION "${SWIFT_INSTALL_PACKAGE_DIR}"
  NO_SET_AND_CHECK_MACRO
  NO_CHECK_REQUIRED_COMPONENTS_MACRO)

write_basic_package_version_file(
  "${swift_cmake_builddir}/SwiftConfigVersion.cmake"
  VERSION "${SWIFT_VERSION}"
  COMPATIBILITY SameMajorVersion)

# Install config + version.
swift_install_in_component(
  FILES
    "${SWIFT_EXPORTS_INSTALL_FILE}"
    "${swift_cmake_builddir}/SwiftConfig.cmake.install"
    "${swift_cmake_builddir}/SwiftConfigVersion.cmake"
  DESTINATION "${SWIFT_INSTALL_PACKAGE_DIR}"
  COMPONENT dev)

# Rename SwiftConfig.cmake.install -> SwiftConfig.cmake on install.
# (CMake's install(FILES) does not rename, so install twice: once with RENAME.)
swift_install_in_component(
  FILES
    "${swift_cmake_builddir}/SwiftConfig.cmake.install"
  DESTINATION "${SWIFT_INSTALL_PACKAGE_DIR}"
  COMPONENT dev
  RENAME "SwiftConfig.cmake")

# Rename SwiftExports.cmake.install -> SwiftExports.cmake on install.
# (CMake's install(FILES) does not rename, so install twice: once with RENAME.)
swift_install_in_component(
  FILES
    "${SWIFT_EXPORTS_INSTALL_FILE}"
  DESTINATION "${SWIFT_INSTALL_PACKAGE_DIR}"
  COMPONENT dev
  RENAME "SwiftExports.cmake")
