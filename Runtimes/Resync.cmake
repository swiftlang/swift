# This CMake script keeps the files in the new standard library build in sync
# with the existing standard library.

# TODO: Once the migration is completed, we can delete this file

cmake_minimum_required(VERSION 3.21)

# Where the standard library lives today
set(StdlibSources "${CMAKE_CURRENT_LIST_DIR}/../stdlib")

message(STATUS "Source dir: ${StdlibSources}")

# Copy the files under the "name" directory in the standard library into the new
# location under Runtimes
function(copy_library_sources name from_prefix to_prefix)
  message(STATUS "${name}[${StdlibSources}/${from_prefix}/${name}] -> ${to_prefix}/${name} ")

  set(full_to_prefix "${CMAKE_CURRENT_LIST_DIR}/${to_prefix}")

  file(GLOB_RECURSE filenames
    FOLLOW_SYMLINKS
    LIST_DIRECTORIES FALSE
    RELATIVE "${StdlibSources}/${from_prefix}"
    "${StdlibSources}/${from_prefix}/${name}/*.swift"
    "${StdlibSources}/${from_prefix}/${name}/*.h"
    "${StdlibSources}/${from_prefix}/${name}/*.cpp"
    "${StdlibSources}/${from_prefix}/${name}/*.c"
    "${StdlibSources}/${from_prefix}/${name}/*.mm"
    "${StdlibSources}/${from_prefix}/${name}/*.m"
    "${StdlibSources}/${from_prefix}/${name}/*.def"
    "${StdlibSources}/${from_prefix}/${name}/*.gyb"
    "${StdlibSources}/${from_prefix}/${name}/*.apinotes"
    "${StdlibSources}/${from_prefix}/${name}/*.yaml"
    "${StdlibSources}/${from_prefix}/${name}/*.inc"
    "${StdlibSources}/${from_prefix}/${name}/*.modulemap"
    "${StdlibSources}/${from_prefix}/${name}/*.json")

  foreach(file ${filenames})
    # Get and create the directory
    get_filename_component(dirname ${file} DIRECTORY)
    file(MAKE_DIRECTORY "${full_to_prefix}/${dirname}")
    file(COPY_FILE
      "${StdlibSources}/${from_prefix}/${file}"         # From
      "${full_to_prefix}/${file}"                       # To
      RESULT _output
      ONLY_IF_DIFFERENT)
    if(_output)
      message(SEND_ERROR
        "Copy ${from_prefix}/${file} -> ${full_to_prefix}/${file} Failed: ${_output}")
    endif()
  endforeach()
endfunction()

# Directories in the existing standard library that make up the Core project

# Copy shared core headers
copy_library_sources(include "" "Core")

# Copy magic linker symbols
copy_library_sources("linker-support" "" "Core")

# Copy Plist
message(STATUS "plist[${StdlibSources}/Info.plist.in] -> Core/Info.plist.in")
file(COPY_FILE
  "${StdlibSources}/Info.plist.in"                             # From
  "${CMAKE_CURRENT_LIST_DIR}/Core/Info.plist.in"               # To
  RESULT _output
  ONLY_IF_DIFFERENT)
if(_output)
  message(SEND_ERROR
    "Copy ${StdlibSources}/Info.plist.in] -> Core/Info.plist.in Failed: ${_output}")
endif()

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

  # Add these as we get them building
  # Demangling

foreach(library ${CoreLibs})
  copy_library_sources(${library} "public" "Core")
endforeach()

# TODO: Add source directories for the platform overlays, supplemental
# libraries, and test support libraries.
