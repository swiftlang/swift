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
project(SwiftDistributed
  LANGUAGES C CXX Swift
  VERSION 6.3.0${BUILD_NUMBER})

if(NOT PROJECT_IS_TOP_LEVEL)
  message(SEND_ERROR "Swift Distributed must build as a standalone project")
endif()

set(CMAKE_Swift_LANGUAGE_VERSION 5)

set(CMAKE_POSITION_INDEPENDENT_CODE YES)

set(CMAKE_C_VISIBILITY_PRESET "hidden")

set(CMAKE_CXX_EXTENSIONS NO)
set(CMAKE_CXX_STANDARD 17 CACHE STRING "C++ standard to conform to")
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_VISIBILITY_PRESET "hidden")

set(CMAKE_VISIBILITY_INLINES_HIDDEN YES)

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
include(ResourceEmbedding)
include(CatalystSupport)
include(SwiftCallingConventions)

option(${PROJECT_NAME}_INSTALL_NESTED_SUBDIR "Install libraries under a platform and architecture subdirectory" ON)
set(${PROJECT_NAME}_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}>" CACHE STRING "")
set(${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}>" CACHE STRING "")

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/Settings.cmake" OPTIONAL)

option(${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION "Generate ABI resilient runtime libraries"
  ${SwiftCore_ENABLE_LIBRARY_EVOLUTION})

option(${PROJECT_NAME}_ENABLE_PRESPECIALIZATION "Enable generic metadata prespecialization"
  ${SwiftCore_ENABLE_PRESPECIALIZATION})

add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-nostdlibimport>
  $<$<COMPILE_LANGUAGE:Swift>:-strict-memory-safety>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NoncopyableGenerics2>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SuppressedAssociatedTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SE427NoInferenceOnExtension>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NonescapableTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature LifetimeDependence>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature InoutLifetimeDependence>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature LifetimeDependenceMutableAccessors>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-upcoming-feature MemberImportVisibility>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enforce-exclusivity=unchecked>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -target-min-inlining-version -Xfrontend min>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-lexical-lifetimes=false>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION}>,$<COMPILE_LANGUAGE:Swift>>:-enable-library-evolution>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_PRESPECIALIZATION}>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xfrontend -prespecialize-generic-metadata>")


add_library(swiftDistributed
  DistributedActor.cpp
  DistributedActor.swift
  DistributedActorSystem.swift
  DistributedAssertions.swift
  DistributedDefaultExecutor.swift
  DistributedMacros.swift
  DistributedMetadata.swift
  LocalTestingDistributedActorSystem.swift)

set_target_properties(swiftDistributed PROPERTIES
  Swift_MODULE_NAME Distributed)
cmake_policy(GET CMP0157 Policy157Enabled)
if(Policy157Enabled STREQUAL NEW)
  set_target_properties(swiftDistributed PROPERTIES
    LINKER_LANGUAGE CXX)
endif()

target_compile_definitions(swiftDistributed PRIVATE
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_TARGET_LIBRARY_NAME=swiftDistributed>)
target_compile_options(swiftDistributed PRIVATE
  $<$<COMPILE_LANGUAGE:Swift>:-parse-stdlib>
  $<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature AllowUnsafeAttribute>)

if(APPLE AND BUILD_SHARED_LIBS)
  target_link_options(swiftDistributed PRIVATE
    "SHELL:-Xlinker -headerpad_max_install_names")
endif()

target_include_directories(swiftDistributed PRIVATE
  # FIXME: Use of `swift/Runtime/...`, `swift/ABI/...`, and `swift/Demangling/...`
  "${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/include"
  # For `swift/runtime/CMakeConfig.h`, which should be generated
  # by the Core build system.
  "${${PROJECT_NAME}_PATH_TO_SWIFT_RUNTIME_HEADERS}"
  # For llvm/support headers
  "${PROJECT_SOURCE_DIR}/include")

target_link_libraries(swiftDistributed PRIVATE
    swiftShims
    swiftCore
    swift_Concurrency
    builtin_float)
    # swiftDarwin/Libc/Platform

install(TARGETS swiftDistributed
    EXPORT SwiftDistributedTargets
    COMPONENT ${PROJECT_NAME}_runtime
    ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
    LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftDistributed)
install_swift_interface(swiftDistributed)

# Configure plist creation for Darwin platforms.
generate_plist("${CMAKE_PROJECT_NAME}" "${CMAKE_PROJECT_VERSION}" swiftDistributed)
embed_manifest(swiftDistributed)

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/swiftDistributed.cmake" OPTIONAL)
