cmake_minimum_required(VERSION 3.29)
# TODO before requiring CMake 4.1 or later
# and/or enforcing CMP0195, please check/update
# the implementation  of `emit_swift_interface`
# in `EmitSwiftInterface.cmake`
# to ensure it keeps laying down nested swiftmodule folders

if(POLICY CMP0157 AND CMAKE_Swift_COMPILER_USE_OLD_DRIVER)
  cmake_policy(SET CMP0157 OLD)
endif()

if($ENV{BUILD_NUMBER})
  math(EXPR BUILD_NUMBER "$ENV{BUILD_NUMBER} % 65535")
  set(BUILD_NUMBER ".${BUILD_NUMBER}")
endif()
project(SwiftDifferentiation
  LANGUAGES Swift C
  VERSION 6.1.0${BUILD_NUMBER})

if(NOT PROJECT_IS_TOP_LEVEL)
  message(SEND_ERROR "Swift Differentiation must build as a standalone project")
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE YES)
set(CMAKE_Swift_LANGUAGE_VERSION 5)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/modules")

set(${PROJECT_NAME}_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

# Hook point for vendor-specific extensions to the build system
# Allowed extension points:
#   - DefaultSettings.cmake
#   - Settings.cmake
set(${PROJECT_NAME}_VENDOR_MODULE_DIR "${CMAKE_SOURCE_DIR}/../cmake/modules/vendor"
  CACHE FILEPATH "Location for private build system extension")

find_package(SwiftCore REQUIRED)
find_package(SwiftOverlay REQUIRED)

include(GNUInstallDirs)

include(AvailabilityMacros)
include(EmitSwiftInterface)
include(InstallSwiftInterface)
include(PlatformInfo)
include(gyb)
include(ResourceEmbedding)
include(CatalystSupport)

option(${PROJECT_NAME}_INSTALL_NESTED_SUBDIR "Install libraries under a platform and architecture subdirectory" ON)
set(${PROJECT_NAME}_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}>" CACHE STRING "")
set(${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}>" CACHE STRING "")

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/Settings.cmake" OPTIONAL)

option(${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION "Generate ABI resilient runtime libraries"
  ${SwiftCore_ENABLE_LIBRARY_EVOLUTION})

option(${PROJECT_NAME}_ENABLE_VECTOR_TYPES "Enable vector support"
  ${SwiftCore_ENABLE_VECTOR_TYPES})


add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-nostdlibimport>
  $<$<COMPILE_LANGUAGE:Swift>:-parse-stdlib>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-library-level api>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NoncopyableGenerics2>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SuppressedAssociatedTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SE427NoInferenceOnExtension>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NonescapableTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature LifetimeDependence>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature MemberImportVisibility>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enforce-exclusivity=unchecked>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -target-min-inlining-version -Xfrontend min>"
  $<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION}>,$<COMPILE_LANGUAGE:Swift>>:-enable-library-evolution>)

if(SwiftDifferentiation_ENABLE_VECTOR_TYPES)
  gyb_expand(SIMDDifferentiation.swift.gyb SIMDDifferentiation.swift)
endif()
gyb_expand(TgmathDerivatives.swift.gyb TgmathDerivatives.swift)
gyb_expand(FloatingPointDifferentiation.swift.gyb
  FloatingPointDifferentiation.swift)

add_library(swift_Differentiation
  AnyDifferentiable.swift
  ArrayDifferentiation.swift
  Differentiable.swift
  DifferentialOperators.swift
  DifferentiationUtilities.swift
  OptionalDifferentiation.swift
  $<$<BOOL:${${PROJECT_NAME}_ENABLE_VECTOR_TYPES}>:SIMDDifferentiation.swift>
  TgmathDerivatives.swift
  FloatingPointDifferentiation.swift
  linker-support/magic-symbols-for-install-name.c)

if(APPLE AND BUILD_SHARED_LIBS)
  target_link_options(swift_Differentiation PRIVATE
    "SHELL:-Xlinker -headerpad_max_install_names")
endif()

set_target_properties(swift_Differentiation PROPERTIES
  Swift_MODULE_NAME _Differentiation)

target_link_libraries(swift_Differentiation PRIVATE
  swiftCore
  $<$<PLATFORM_ID:Android>:swiftAndroid>
  $<$<PLATFORM_ID:Windows>:swiftCRT>)


install(TARGETS swift_Differentiation
  EXPORT SwiftSupplementalTargets
  COMPONENT ${PROJECT_NAME}_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swift_Differentiation)
install_swift_interface(swift_Differentiation)

# Configure plist creation for Darwin platforms.
generate_plist("${CMAKE_PROJECT_NAME}" "${CMAKE_PROJECT_VERSION}" swift_Differentiation)
embed_manifest(swift_Differentiation)

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/swift_Differentiation.cmake" OPTIONAL)
