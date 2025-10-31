cmake_minimum_required(VERSION 3.26...3.29)
# TODO before requiring CMake 4.1 or later
# and/or enforcing CMP0195, please check/update
# the implementation  of `emit_swift_interface`
# in `EmitSwiftInterface.cmake`
# to ensure it keeps laying down nested swiftmodule folders

set(CMAKE_C_VISIBILITY_PRESET "hidden")
set(CMAKE_CXX_VISIBILITY_PRESET "hidden")
set(CMAKE_VISIBILITY_INLINES_HIDDEN YES)

list(APPEND CMAKE_MODULE_PATH
  "${CMAKE_SOURCE_DIR}/cmake/modules"
  "${CMAKE_SOURCE_DIR}/../../cmake/modules")

if(POLICY CMP0157 AND CMAKE_Swift_COMPILER_USE_OLD_DRIVER)
  cmake_policy(SET CMP0157 OLD)
endif()

include(SwiftProjectVersion)
project(SwiftClientRetainRelease
  LANGUAGES ASM
  VERSION ${SWIFT_RUNTIME_VERSION})


set(${PROJECT_NAME}_VENDOR_MODULE_DIR "${CMAKE_SOURCE_DIR}/cmake/modules/vendor"
  CACHE FILEPATH "Location for private build system extension")

include(GNUInstallDirs)

include(PlatformInfo)
include(CatalystSupport)

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/Settings.cmake" OPTIONAL)

add_compile_options(
  $<$<BOOL:${SwiftRetainReleaseClient_STDLIB_ENABLE_OBJC_INTEROP}>:-DSWIFT_OBJC_INTEROP=1>)

# LNK4049: symbol 'symbol' defined in 'filename.obj' is imported
# LNK4286: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj'
# LNK4217: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj' in function 'function'
#
# We cannot selectively filter the linker warnings as we do not use the MSVC
# frontned and `clang-cl` (and `clang`) currently do not support `/WX:nnnn`. As
# a compromise, treat all linker warnings as errors.
add_link_options($<$<PLATFORM_ID:Windows>:LINKER:/WX>)
# Ensure all symbols are fully resolved on Linux
add_link_options($<$<PLATFORM_ID:Android,Linux>:LINKER:-z,defs>)

set(SwiftCore_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

add_library(swiftClientRetainRelease
  STATIC  RetainRelease.s)

# FIXME: swift/ABI/System.h from the compiler
#        headers, which imports System.h from the shims
#        We should split out the parts that are needed by the runtime
#        to avoid pulling in headers from the compiler.
target_include_directories(swiftClientRetainRelease PRIVATE
  "${SwiftCore_SWIFTC_SOURCE_DIR}/include"
  "${CMAKE_SOURCE_DIR}/../../Core/SwiftShims")

install(TARGETS swiftClientRetainRelease
  EXPORT SwiftClientRetainReleaseTargets
  COMPONENT ${PROJECT_NAME}_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")

# Inter-project install info
export(EXPORT SwiftClientRetainReleaseTargets
  FILE "cmake/SwiftClientRetainRelease/SwiftOverlayTargets.cmake")
install(EXPORT SwiftClientRetainReleaseTargets
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftClientRetainRelease"
  FILE "SwiftClientRetainReleaseTargets.cmake"
  COMPONENT SwiftClientRetainReleaseCMake)
include(CMakePackageConfigHelpers)
configure_package_config_file("${CMAKE_CURRENT_SOURCE_DIR}/cmake/interface/SwiftClientRetainReleaseConfig.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftClientRetainRelease/SwiftOverlayConfig.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftClientRetainRelease")
write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftClientRetainRelease/SwiftOverlayConfigVersion.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY ExactVersion)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftClientRetainRelease/SwiftOverlayConfig.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftClientRetainRelease/SwiftOverlayConfigVersion.cmake"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftClientRetainRelease")

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/swiftClientRetainRelease.cmake" OPTIONAL)
