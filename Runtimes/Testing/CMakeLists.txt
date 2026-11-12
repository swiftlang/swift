cmake_minimum_required(VERSION 3.29)
# TODO before requiring CMake 4.1 or later
# and/or enforcing CMP0195, please check/update
# the implementation  of `emit_swift_interface`
# in `EmitSwiftInterface.cmake`
# to ensure it keeps laying down nested swiftmodule folders

# StdlibUnittest and the libraries it uses, built against a Swift SDK.

list(APPEND CMAKE_MODULE_PATH
  "${CMAKE_SOURCE_DIR}/../Supplemental/cmake/modules"
  "${CMAKE_SOURCE_DIR}/../cmake/modules")

include(LanguageVersions)
include(SwiftProjectVersion)
project(SwiftTestSupport
  LANGUAGES C CXX Swift
  VERSION ${SWIFT_RUNTIME_VERSION})

if(NOT PROJECT_IS_TOP_LEVEL)
  message(SEND_ERROR "Swift test support must build as a standalone project")
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE YES)

set(${PROJECT_NAME}_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

set(Swift_SDKROOT "$ENV{SDKROOT}" CACHE PATH "Swift SDK under test")
if(NOT Swift_SDKROOT)
  message(FATAL_ERROR "Set Swift_SDKROOT to the Swift SDK under test")
endif()
file(TO_CMAKE_PATH "${Swift_SDKROOT}" Swift_SDKROOT)

include(GNUInstallDirs)

include(AvailabilityMacros)
include(EmitSwiftInterface)
include(InstallSwiftInterface)
include(PlatformInfo)
include(gyb)

option(${PROJECT_NAME}_INSTALL_NESTED_SUBDIR "Install libraries under a platform and architecture subdirectory" ON)
set(${PROJECT_NAME}_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}>" CACHE STRING "")
set(${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}>" CACHE STRING "")

option(${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION "Generate ABI resilient libraries" NO)
option(${PROJECT_NAME}_ENABLE_REFLECTION "Enable runtime support for mirrors and reflection support" YES)
option(${PROJECT_NAME}_ENABLE_CONCURRENCY "Enable Concurrency runtime support" YES)
option(${PROJECT_NAME}_ENABLE_STRING_PROCESSING "Enable string processing support" YES)
option(${PROJECT_NAME}_ENABLE_DIFFERENTIATION "Enable differentiable programming support" YES)
option(${PROJECT_NAME}_ENABLE_ENVIRONMENT "Enable environment variable support" YES)
option(${PROJECT_NAME}_ENABLE_COMMANDLINE_SUPPORT "Enable command line argument support" YES)
option(${PROJECT_NAME}_ENABLE_RUNTIME_LEAK_CHECKER "Enable the runtime leak checker" NO)

if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
  set(${PROJECT_NAME}_THREADING_PACKAGE "WIN32")
elseif(CMAKE_SYSTEM_NAME MATCHES "Darwin")
  set(${PROJECT_NAME}_THREADING_PACKAGE "DARWIN")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux" OR CMAKE_SYSTEM_NAME STREQUAL "Android")
  set(${PROJECT_NAME}_THREADING_PACKAGE "LINUX")
else()
  set(${PROJECT_NAME}_THREADING_PACKAGE "PTHREADS")
endif()

set(CMAKE_Swift_LANGUAGE_VERSION 5)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)

add_compile_definitions(
  $<$<COMPILE_LANGUAGE:C,CXX>:SWIFT_RUNTIME>
  $<$<COMPILE_LANGUAGE:C,CXX>:SWIFT_INLINE_NAMESPACE=__runtime>
  $<$<COMPILE_LANGUAGE:C,CXX>:SWIFT_THREADING_${${PROJECT_NAME}_THREADING_PACKAGE}>
  $<$<COMPILE_LANGUAGE:C,CXX>:SWIFT_RUNTIME_ENABLE_LEAK_CHECKER=$<BOOL:${${PROJECT_NAME}_ENABLE_RUNTIME_LEAK_CHECKER}>>
  $<$<AND:$<COMPILE_LANGUAGE:C,CXX>,$<BOOL:${${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION}>>:SWIFT_LIBRARY_EVOLUTION=1>
  $<$<BOOL:${${PROJECT_NAME}_ENABLE_REFLECTION}>:SWIFT_ENABLE_REFLECTION>
  $<$<BOOL:${${PROJECT_NAME}_ENABLE_ENVIRONMENT}>:SWIFT_STDLIB_HAS_ENVIRON>
  $<$<BOOL:${${PROJECT_NAME}_ENABLE_COMMANDLINE_SUPPORT}>:SWIFT_STDLIB_HAS_COMMANDLINE>
  $<$<AND:$<COMPILE_LANGUAGE:Swift>,$<BOOL:${${PROJECT_NAME}_ENABLE_CONCURRENCY}>>:SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY>
  $<$<AND:$<COMPILE_LANGUAGE:Swift>,$<BOOL:${${PROJECT_NAME}_ENABLE_RUNTIME_LEAK_CHECKER}>>:SWIFT_RUNTIME_ENABLE_LEAK_CHECKER>)

add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-sdk$<SEMICOLON>${Swift_SDKROOT}>
  $<$<COMPILE_LANGUAGE:CXX>:-fno-rtti>
  $<$<COMPILE_LANGUAGE:CXX>:-fno-exceptions>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SuppressedAssociatedTypesWithDefaults>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NonescapableTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Lifetimes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature AddressableParameters>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enforce-exclusivity=unchecked>"
  "$<$<COMPILE_LANGUAGE:Swift>:-warn-implicit-overrides>"
  "$<$<AND:$<NOT:$<PLATFORM_ID:Darwin>>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xfrontend -disable-objc-interop>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION}>,$<COMPILE_LANGUAGE:Swift>>:-enable-library-evolution>")

add_link_options($<$<LINK_LANGUAGE:Swift>:-sdk$<SEMICOLON>${Swift_SDKROOT}>)

include_directories(
  # FIXME: Use of `swift/Runtime/...`, `swift/ABI/...`, and `swift/Basic/...`
  "${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/include"
  # For `swift/shims/...`
  "${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/stdlib/public/SwiftShims"
  # For llvm/support headers
  "${PROJECT_SOURCE_DIR}/include")

function(add_test_support_library name)
  cmake_parse_arguments(PARSE_ARGV 1 ARG "" "MODULE_NAME" "")
  add_library(${name} ${ARG_UNPARSED_ARGUMENTS})
  set_target_properties(${name} PROPERTIES
    Swift_MODULE_NAME ${ARG_MODULE_NAME})
  target_compile_definitions(${name} PRIVATE
    $<$<COMPILE_LANGUAGE:C,CXX>:SWIFT_TARGET_LIBRARY_NAME=${name}>)
  emit_swift_interface(${name})
  install_swift_interface(${name})
  install(TARGETS ${name}
    EXPORT SwiftTestSupportTargets
    COMPONENT ${PROJECT_NAME}_runtime
    ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
    LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
endfunction()

gyb_expand(SwiftPrivate/AtomicInt.swift.gyb SwiftPrivate/AtomicInt.swift)
add_test_support_library(swiftSwiftPrivate
  MODULE_NAME SwiftPrivate
  SwiftPrivate/SwiftPrivate.swift
  SwiftPrivate/IO.swift
  SwiftPrivate/ShardedAtomicCounter.swift
  "${CMAKE_CURRENT_BINARY_DIR}/SwiftPrivate/AtomicInt.swift")
target_compile_options(swiftSwiftPrivate PRIVATE
  "$<$<COMPILE_LANGUAGE:Swift>:-parse-stdlib>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-access-control>")

add_test_support_library(swiftSwiftPrivateThreadExtras
  MODULE_NAME SwiftPrivateThreadExtras
  SwiftPrivateThreadExtras/SwiftPrivateThreadExtras.swift
  SwiftPrivateThreadExtras/ThreadBarriers.swift)

add_test_support_library(swiftSwiftPrivateLibcExtras
  MODULE_NAME SwiftPrivateLibcExtras
  SwiftPrivateLibcExtras/SwiftPrivateLibcExtras.swift
  SwiftPrivateLibcExtras/Subprocess.c
  SwiftPrivateLibcExtras/Subprocess.swift)
target_link_libraries(swiftSwiftPrivateLibcExtras PRIVATE
  swiftSwiftPrivate)

add_test_support_library(swiftStdlibUnittest
  MODULE_NAME StdlibUnittest
  StdlibUnittest/StdlibUnittest.swift
  StdlibUnittest/CheckStrideable.swift
  StdlibUnittest/InspectValue.cpp
  StdlibUnittest/InspectValue.swift
  StdlibUnittest/InterceptTraps.cpp
  StdlibUnittest/LifetimeTracked.swift
  StdlibUnittest/MinimalTypes.swift
  StdlibUnittest/OpaqueIdentityFunctions.cpp
  StdlibUnittest/OpaqueIdentityFunctions.swift
  StdlibUnittest/RaceTest.swift
  StdlibUnittest/Statistics.swift
  StdlibUnittest/StdlibCoreExtras.swift
  StdlibUnittest/StringConvertible.swift
  StdlibUnittest/StringTestHelpers.swift
  StdlibUnittest/TestHelpers.swift
  StdlibUnittest/TypeIndexed.swift)
target_compile_options(swiftStdlibUnittest PRIVATE
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-objc-attr-requires-foundation-module>"
  # Let `Builtin.assert_configuration` return the active value from the client
  # module in `@export(implementation)` functions, like for the stdlib.
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -assert-config -Xfrontend DisableReplacement>"
  "$<$<AND:$<COMPILE_LANGUAGE:Swift>,$<CONFIG:Debug>>:SHELL:-D SWIFT_STDLIB_DEBUG>")
target_link_libraries(swiftStdlibUnittest PRIVATE
  swiftSwiftPrivate
  swiftSwiftPrivateThreadExtras
  swiftSwiftPrivateLibcExtras)

add_test_support_library(swiftStdlibCollectionUnittest
  MODULE_NAME StdlibCollectionUnittest
  StdlibCollectionUnittest/StdlibCollectionUnittest.swift
  StdlibCollectionUnittest/CheckCollectionInstance.swift
  StdlibCollectionUnittest/CheckCollectionType.swift
  StdlibCollectionUnittest/CheckMutableCollectionType.swift
  StdlibCollectionUnittest/CheckRangeReplaceableCollectionType.swift
  StdlibCollectionUnittest/CheckRangeReplaceableSliceType.swift
  StdlibCollectionUnittest/CheckSequenceInstance.swift
  StdlibCollectionUnittest/CheckSequenceType.swift
  StdlibCollectionUnittest/COWLoggingArray.swift
  StdlibCollectionUnittest/LoggingWrappers.swift
  StdlibCollectionUnittest/MinimalCollections.swift
  StdlibCollectionUnittest/RangeSelection.swift
  StdlibCollectionUnittest/WriteBackMutableSlice.swift)
target_link_libraries(swiftStdlibCollectionUnittest PRIVATE
  swiftStdlibUnittest)

add_test_support_library(swiftStdlibUnicodeUnittest
  MODULE_NAME StdlibUnicodeUnittest
  StdlibUnicodeUnittest/StdlibUnicodeUnittest.swift
  StdlibUnicodeUnittest/Collation.swift
  StdlibUnicodeUnittest/UnicodeScalarProperties.swift
  StdlibUnicodeUnittest/GraphemeBreaking.swift
  StdlibUnicodeUnittest/WordBreaking.swift)
target_link_libraries(swiftStdlibUnicodeUnittest PRIVATE
  swiftStdlibUnittest)

add_test_support_library(swiftRuntimeUnittest
  MODULE_NAME RuntimeUnittest
  RuntimeUnittest/RuntimeUnittest.swift
  RuntimeUnittest/ExclusivityTests.cpp)
target_link_libraries(swiftRuntimeUnittest PRIVATE
  swiftStdlibUnittest)

if(${PROJECT_NAME}_ENABLE_DIFFERENTIATION)
  gyb_expand(DifferentiationUnittest/DifferentiationUnittest.swift.gyb
    DifferentiationUnittest/DifferentiationUnittest.swift)
  add_test_support_library(swiftDifferentiationUnittest
    MODULE_NAME DifferentiationUnittest
    "${CMAKE_CURRENT_BINARY_DIR}/DifferentiationUnittest/DifferentiationUnittest.swift")
  # The tests rely on `Tracked` being resilient.
  target_compile_options(swiftDifferentiationUnittest PRIVATE
    "$<$<COMPILE_LANGUAGE:Swift>:-enable-library-evolution>")
  target_link_libraries(swiftDifferentiationUnittest PRIVATE
    swiftStdlibUnittest)
endif()
