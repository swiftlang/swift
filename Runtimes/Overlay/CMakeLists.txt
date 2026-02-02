
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
  "${CMAKE_SOURCE_DIR}/../cmake/modules")

if(POLICY CMP0157 AND CMAKE_Swift_COMPILER_USE_OLD_DRIVER)
  cmake_policy(SET CMP0157 OLD)
endif()

include(SwiftProjectVersion)
project(SwiftOverlay
  LANGUAGES C CXX Swift
  VERSION ${SWIFT_RUNTIME_VERSION})

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
defaulted_option(SwiftOverlay_ENABLE_CXX_INTEROP "Enable C++ Interop support overlays")

option(SwiftOverlay_INSTALL_NESTED_SUBDIR "Install libraries under a platform and architecture subdirectory" ON)
set(SwiftOverlay_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${SwiftOverlay_INSTALL_NESTED_SUBDIR}>:/${SwiftOverlay_PLATFORM_SUBDIR}/${SwiftOverlay_ARCH_SUBDIR}>")
set(SwiftOverlay_INSTALL_SWIFTMODULEDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${SwiftOverlay_INSTALL_NESTED_SUBDIR}>:/${SwiftOverlay_PLATFORM_SUBDIR}>")

option(SwiftOverlay_ENABLE_LIBRARY_EVOLUTION "Generate ABI resilient runtime libraries"
  ${SwiftCore_ENABLE_LIBRARY_EVOLUTION})
option(SwiftOverlay_ENABLE_BACKDEPLOYMENT_SUPPORT "Add symbols for runtime backdeployment"
  ${SwiftCore_ENABLE_BACKDEPLOYMENT_SUPPORT})
option(${PROJECT_NAME}_ENABLE_DIRECT_RETAIN_RELEASE "Use direct retain release in overlays"
  ${SwiftCore_ENABLE_DIRECT_RETAIN_RELEASE})

add_compile_definitions(
  $<$<BOOL:${SwiftOverlay_ENABLE_BACKDEPLOYMENT_SUPPORT}>:SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT>)

add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-nostdlibimport>
  $<$<COMPILE_LANGUAGE:Swift>:-strict-memory-safety>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-lexical-lifetimes=false>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-concurrency-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-string-processing-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enforce-exclusivity=unchecked>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -target-min-inlining-version -Xfrontend min>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION}>,$<COMPILE_LANGUAGE:Swift>>:-enable-library-evolution>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_PRESPECIALIZATION}>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xfrontend -prespecialize-generic-metadata>")

if(${PROJECT_NAME}_ENABLE_DIRECT_RETAIN_RELEASE)
  find_package(SwiftSwiftDirectRuntime REQUIRED)
  add_compile_options("$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-direct-retain-release>")
endif()

include(ExperimentalFeatures)

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

include(ExperimentalFeatures)

if(NOT LINUX)
  # explicit module builds crash the Swift compiler when building the Glibc
  # platform overlay https://github.com/swiftlang/swift/issues/84740
  add_compile_options($<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>)
endif()

add_subdirectory(clang)
if(ANDROID)
  add_subdirectory(Android)
elseif(WIN32)
  add_subdirectory(Windows)
elseif(LINUX)
  defaulted_set(SwiftOverlay_LINUX_LIBC STRING "Select Linux libc flavor. Options are: glibc")
  add_subdirectory("Linux/${SwiftOverlay_LINUX_LIBC}")
endif()

if(SwiftOverlay_ENABLE_CXX_INTEROP)
  add_subdirectory(Cxx)
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
