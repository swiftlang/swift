
cmake_minimum_required(VERSION 3.26...3.29)
# TODO before requiring CMake 4.1 or later
# and/or enforcing CMP0195, please check/update
# the implementation  of `emit_swift_interface`
# in `EmitSwiftInterface.cmake`
# to ensure it keeps laying down nested swiftmodule folders

set(CMAKE_C_VISIBILITY_PRESET "hidden")
set(CMAKE_CXX_VISIBILITY_PRESET "hidden")
set(CMAKE_VISIBILITY_INLINES_HIDDEN YES)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/modules")

if(POLICY CMP0157 AND CMAKE_Swift_COMPILER_USE_OLD_DRIVER)
  cmake_policy(SET CMP0157 OLD)
endif()

# NOTE: always use the 3-component style as the expansion as
# `${PROJECT_VERSION}` will not extend this to the complete form and this can
# change the behaviour for comparison with non-SemVer compliant parsing. If
# possible, use the 4-version component as that is used to differentiate the
# builds of the runtime for Windows.
if($ENV{BUILD_NUMBER})
  # NOTE: SxS modules have a limit on each component being [0-65535].
  # https://learn.microsoft.com/en-us/windows/win32/sbscs/assembly-versions
  math(EXPR BUILD_NUMBER "$ENV{BUILD_NUMBER} % 65535")
  set(BUILD_NUMBER ".${BUILD_NUMBER}")
endif()
project(SwiftOverlay
  LANGUAGES C CXX Swift
  VERSION 6.1.0${BUILD_NUMBER})

set(CMAKE_Swift_LANGUAGE_VERSION 5)

find_package(SwiftCore)

# FIXME: We should not need to refer back into the compiler sources. This is
# needed by gyb and AvailabilityMacros
set(SwiftOverlay_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

set(${PROJECT_NAME}_VENDOR_MODULE_DIR "${CMAKE_SOURCE_DIR}/cmake/modules/vendor"
  CACHE FILEPATH "Location for private build system extension")

include(GNUInstallDirs)

include(gyb)
include(AvailabilityMacros)
include(DefaultSettings)
include(EmitSwiftInterface)
include(InstallSwiftInterface)
include(PlatformInfo)
include(ResourceEmbedding)

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/Settings.cmake" OPTIONAL)

defaulted_option(SwiftOverlay_ENABLE_REFLECTION "Enable runtime support for mirrors and reflection support")

option(SwiftOverlay_INSTALL_NESTED_SUBDIR "Install libraries under a platform and architecture subdirectory" ON)
set(SwiftOverlay_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${SwiftOverlay_INSTALL_NESTED_SUBDIR}>:/${SwiftOverlay_PLATFORM_SUBDIR}/${SwiftOverlay_ARCH_SUBDIR}>")
set(SwiftOverlay_INSTALL_SWIFTMODULEDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${SwiftOverlay_INSTALL_NESTED_SUBDIR}>:/${SwiftOverlay_PLATFORM_SUBDIR}>")

option(SwiftOverlay_ENABLE_LIBRARY_EVOLUTION "Generate ABI resilient runtime libraries"
  ${SwiftCore_ENABLE_LIBRARY_EVOLUTION})
option(SwiftOverlay_ENABLE_BACKDEPLOYMENT_SUPPORT "Add symbols for runtime backdeployment"
  ${SwiftCore_ENABLE_BACKDEPLOYMENT_SUPPORT})

add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-nostdlibimport>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enforce-exclusivity=unchecked>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -target-min-inlining-version -Xfrontend min>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-lexical-lifetimes=false>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-ossa-modules>"
  "$<$<AND:$<BOOL:${SwiftOverlay_ENABLE_LIBRARY_EVOLUTION}>,$<COMPILE_LANGUAGE:Swift>>:-enable-library-evolution>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-concurrency-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-string-processing-module-import>")

# LNK4049: symbol 'symbol' defined in 'filename.obj' is imported
# LNK4286: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj'
# LNK4217: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj' in function 'function'
#
# We cannot selectively filter the linker warnings as we do not use the MSVC
# frontned and `clang-cl` (and `clang`) currently do not support `/WX:nnnn`. As
# a compromise, treat all linker warnings as errors.
add_link_options($<$<PLATFORM_ID:Windows>:LINKER:/WX>)

add_compile_definitions(
  $<$<BOOL:${SwiftOverlay_ENABLE_BACKDEPLOYMENT_SUPPORT}>:SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT>)

include(ExperimentalFeatures)

add_subdirectory(clang)
if(ANDROID)
  add_subdirectory(Android)
endif()
if(WIN32)
  add_subdirectory(Windows)
endif()

# Inter-project install info
export(EXPORT SwiftOverlayTargets
  FILE "cmake/SwiftOverlay/SwiftOverlayTargets.cmake")
install(EXPORT SwiftOverlayTargets
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftOverlay"
  FILE "SwiftOverlayTargets.cmake"
  COMPONENT SwiftOverlayCMake)
include(CMakePackageConfigHelpers)
configure_package_config_file("${CMAKE_CURRENT_SOURCE_DIR}/cmake/interface/SwiftOverlayConfig.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftOverlay/SwiftOverlayConfig.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftOverlay")
write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftOverlay/SwiftOverlayConfigVersion.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY ExactVersion)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftOverlay/SwiftOverlayConfig.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftOverlay/SwiftOverlayConfigVersion.cmake"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftOverlay")

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/swiftOverlay.cmake" OPTIONAL)
