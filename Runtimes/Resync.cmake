# This CMake script keeps the files in the new standard library build in sync
# with the existing standard library.

# TODO: Once the migration is completed, we can delete this file

cmake_minimum_required(VERSION 3.21)

# Where the standard library lives today
set(StdlibSources "${CMAKE_CURRENT_LIST_DIR}/../stdlib")

# Copy a list of files
function(copy_files from_prefix to_prefix)
  cmake_parse_arguments(ARG "" "ROOT" "FILES" ${ARGN})
  if(NOT ARG_ROOT)
    set(ARG_ROOT ${StdlibSources})
  endif()

  set(full_to_prefix "${CMAKE_CURRENT_LIST_DIR}/${to_prefix}")

  foreach(file ${ARG_FILES})
    # Get and create the directory
    get_filename_component(dirname ${file} DIRECTORY)
    file(MAKE_DIRECTORY "${full_to_prefix}/${dirname}")
    file(COPY_FILE
      "${ARG_ROOT}/${from_prefix}/${file}"              # From
      "${full_to_prefix}/${file}"                       # To
      RESULT _output
      ONLY_IF_DIFFERENT)
    if(_output)
      message(SEND_ERROR
        "Copy ${ARG_ROOT}/${from_prefix}/${file} -> ${full_to_prefix}/${file} Failed: ${_output}")
    endif()
  endforeach()
endfunction()

# Copy the files under the "name" directory in the standard library into the new
# location under Runtimes
function(copy_library_sources name from_prefix to_prefix)
  cmake_parse_arguments(ARG "" "ROOT" "" ${ARGN})

  if(NOT ARG_ROOT)
    set(ARG_ROOT ${StdlibSources})
  endif()

  message(STATUS "${name}[${ARG_ROOT}/${from_prefix}/${name}] -> ${to_prefix}/${name} ")

  set(full_to_prefix "${CMAKE_CURRENT_LIST_DIR}/${to_prefix}")

  file(GLOB_RECURSE filenames
    FOLLOW_SYMLINKS
    LIST_DIRECTORIES FALSE
    RELATIVE "${ARG_ROOT}/${from_prefix}"
    "${ARG_ROOT}/${from_prefix}/${name}/*.swift"
    "${ARG_ROOT}/${from_prefix}/${name}/*.h"
    "${ARG_ROOT}/${from_prefix}/${name}/*.cpp"
    "${ARG_ROOT}/${from_prefix}/${name}/*.c"
    "${ARG_ROOT}/${from_prefix}/${name}/*.mm"
    "${ARG_ROOT}/${from_prefix}/${name}/*.m"
    "${ARG_ROOT}/${from_prefix}/${name}/*.def"
    "${ARG_ROOT}/${from_prefix}/${name}/*.gyb"
    "${ARG_ROOT}/${from_prefix}/${name}/*.apinotes"
    "${ARG_ROOT}/${from_prefix}/${name}/*.yaml"
    "${ARG_ROOT}/${from_prefix}/${name}/*.inc"
    "${ARG_ROOT}/${from_prefix}/${name}/*.modulemap"
    "${ARG_ROOT}/${from_prefix}/${name}/*.json")

  copy_files("${from_prefix}" "${to_prefix}"
    ROOT "${ARG_ROOT}"
    FILES ${filenames})
endfunction()

# Directories in the existing standard library that make up the Core project

# Copy shared core headers
copy_library_sources(include "" "Core")

# Copy magic linker symbols
copy_library_sources("linker-support" "" "Core")

set(CoreLibs
  LLVMSupport
  SwiftShims
  runtime
  CompatibilityOverride
  stubs
  CommandLineSupport
  core
  SwiftOnoneSupport
  Concurrency
  Concurrency/InternalShims)

foreach(library ${CoreLibs})
  copy_library_sources(${library} "public" "Core")
endforeach()

message(STATUS "plist[${StdlibSources}/Info.plist.in] -> Core/Info.plist.in")
copy_files("" "Core" FILES "Info.plist.in")

message(STATUS "plist[${StdlibSources}/Info.plist.in] -> Supplemental/Synchronization/Info.plist.in")
copy_files("" "Supplemental/Synchronization" FILES "Info.plist.in")

message(STATUS "plist[${StdlibSources}/Info.plist.in] -> Supplemental/Distributed/Info.plist.in")
copy_files("" "Supplemental/Distributed" FILES "Info.plist.in")

# Platform Overlays

# Copy magic linker symbols
copy_library_sources("linker-support" "" "Overlay")

message(STATUS "Clang[${StdlibSources}/public/ClangOverlays] -> ${CMAKE_CURRENT_LIST_DIR}/Overlay/clang")
copy_files(public/ClangOverlays Overlay/clang FILES float.swift.gyb)

# Android Overlay
message(STATUS "Android modulemaps[${StdlibSources}/Platform] -> ${CMAKE_CURRENT_LIST_DIR}/Overlay/Android/clang")
copy_files(public/Platform Overlay/Android/clang
  FILES
    android.modulemap
    posix_filesystem.apinotes
    spawn.apinotes
    SwiftAndroidNDK.h
    SwiftBionic.h)

message(STATUS "Android Android[${StdlibSources}/Platform] -> ${CMAKE_CURRENT_LIST_DIR}/Overlay/Android/Android")
copy_files(public/Platform Overlay/Android/Android
  FILES
    Android.swift
    Platform.swift
    POSIXError.swift
    TiocConstants.swift
    tgmath.swift.gyb)

message(STATUS "Android Math[${StdlibSources}/Platform] -> ${CMAKE_CURRENT_LIST_DIR}/Overlay/Android/Math")
copy_files(public/Platform Overlay/Android/Math
  FILES
    Math.swift)

# Windows Overlay
message(STATUS "WinSDK[${StdlibSources}/public/Windows] -> ${CMAKE_CURRENT_LIST_DIR}/Overlay/Windows/WinSDK")
copy_files(public/Windows Overlay/Windows/WinSDK FILES WinSDK.swift)

message(STATUS "Windows modulemaps[${StdlibSources}/Platform] -> ${CMAKE_CURRENT_LIST_DIR}/Overlay/Windows/clang")
copy_files(public/Platform Overlay/Windows/clang
  FILES
    ucrt.modulemap
    winsdk.modulemap
    vcruntime.modulemap
    vcruntime.apinotes)

message(STATUS "CRT[${StdlibSources}/public/Platform] -> ${CMAKE_CURRENT_LIST_DIR}/Overlay/Windows/CRT")
copy_files(public/Platform Overlay/Windows/CRT
  FILES
    ucrt.swift
    Platform.swift
    POSIXError.swift
    TiocConstants.swift
    tgmath.swift.gyb)

# TODO: Add source directories for the platform overlays, supplemental
# libraries, and test support libraries.

# Supplemental Libraries
copy_library_sources("Synchronization" "public" "Supplemental")


# Copy StringProcessing, RegexParser, RegexBuilder
if(NOT DEFINED StringProcessing_ROOT_DIR)
  find_path(StringProcessing_ROOT_DIR
    "swift-experimental-string-processing/Package.swift"
    HINTS "${CMAKE_CURRENT_LIST_DIR}/../../")
endif()
message(STATUS "String Processing Root: ${StringProcessing_ROOT_DIR}")

copy_library_sources(_RegexParser "Sources" "Supplemental/StringProcessing"
  ROOT "${StringProcessing_ROOT_DIR}/swift-experimental-string-processing")
copy_library_sources(_StringProcessing "Sources" "Supplemental/StringProcessing"
  ROOT "${StringProcessing_ROOT_DIR}/swift-experimental-string-processing")
copy_library_sources(_CUnicode "Sources" "Supplemental/StringProcessing/_StringProcessing"
  ROOT "${StringProcessing_ROOT_DIR}/swift-experimental-string-processing")
copy_library_sources(RegexBuilder "Sources" "Supplemental/StringProcessing"
  ROOT "${StringProcessing_ROOT_DIR}/swift-experimental-string-processing")

copy_library_sources("Distributed" "public" "Supplemental")
copy_library_sources(include "" "Supplemental/Distributed")
