
# On non-Darwin require UUID.
if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set(UUID_INCLUDE "")
  set(UUID_LIBRARIES "")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Windows")
  set(UUID_INCLUDE "")
  set(UUID_LIBRARIES "rpcrt4.lib")
else()
  find_package(UUID REQUIRED)
  set(UUID_INCLUDE "${UUID_INCLUDE_DIRS}")
endif()

# Figure out if we can track VC revisions.
# FIXME: Copied from Clang.
function(find_first_existing_file out_var)
  foreach(file ${ARGN})
    if(EXISTS "${file}")
      set(${out_var} "${file}" PARENT_SCOPE)
      return()
    endif()
  endforeach()
endfunction()

macro(find_first_existing_vc_file out_var path)
  find_first_existing_file(${out_var}
    "${path}/.git/logs/HEAD" # Git
    "${path}/.svn/wc.db"     # SVN 1.7
    "${path}/.svn/entries"   # SVN 1.6
    )
endmacro()

set(generate_vcs_version_script "${LLVM_MAIN_SRC_DIR}/cmake/modules/GenerateVersionFromVCS.cmake")

function(generate_revision_inc revision_inc_var name dir)
  find_first_existing_vc_file(dep_file "${dir}")
  # Create custom target to generate the VC revision include.
  set(revision_inc "${CMAKE_CURRENT_BINARY_DIR}/${name}Revision.inc")
  string(TOUPPER ${name} upper_name)
  if(DEFINED dep_file)
    add_custom_command(OUTPUT "${revision_inc}"
      DEPENDS "${dep_file}" "${generate_vcs_version_script}"
      COMMAND
      ${CMAKE_COMMAND} "-DNAMES=${upper_name}"
                       "-D${upper_name}_SOURCE_DIR=${dir}"
                       "-DHEADER_FILE=${revision_inc}"
                       -P "${generate_vcs_version_script}")
  else()
    # Generate an empty Revision.inc file if we are not using git or SVN.
    file(WRITE "${revision_inc}" "")
  endif()

  # Mark the generated header as being generated.
  set_source_files_properties("${revision_inc}"
    PROPERTIES GENERATED TRUE
               HEADER_FILE_ONLY TRUE)
  set(${revision_inc_var} ${revision_inc} PARENT_SCOPE)
endfunction()

generate_revision_inc(llvm_revision_inc LLVM "${LLVM_MAIN_SRC_DIR}")
generate_revision_inc(clang_revision_inc Clang "${CLANG_MAIN_SRC_DIR}")
generate_revision_inc(swift_revision_inc Swift "${SWIFT_SOURCE_DIR}")

set(version_inc_files
  ${llvm_revision_inc} ${clang_revision_inc} ${swift_revision_inc})

add_swift_host_library(swiftBasic STATIC
  AnyValue.cpp
  Cache.cpp
  ClusteredBitVector.cpp
  DiverseStack.cpp
  Edit.cpp
  EditorPlaceholder.cpp
  ExponentialGrowthAppendingBinaryByteStream.cpp
  FileSystem.cpp
  FileTypes.cpp
  JSONSerialization.cpp
  LangOptions.cpp
  LLVMContext.cpp
  Mangler.cpp
  OutputFileMap.cpp
  Platform.cpp
  PrefixMap.cpp
  PrettyStackTrace.cpp
  PrimitiveParsing.cpp
  Program.cpp
  QuotedString.cpp
  SourceLoc.cpp
  Statistic.cpp
  StringExtras.cpp
  TaskQueue.cpp
  ThreadSafeRefCounted.cpp
  Timer.cpp
  Unicode.cpp
  UUID.cpp
  Version.cpp
  ${version_inc_files}

  # Platform-specific TaskQueue implementations
  Unix/TaskQueue.inc

  # Platform-agnostic fallback TaskQueue implementation
  Default/TaskQueue.inc

  GYB_SOURCES
    UnicodeExtendedGraphemeClusters.cpp.gyb

  LLVM_LINK_COMPONENTS support)
target_include_directories(swiftBasic PRIVATE
  ${UUID_INCLUDE})

target_link_libraries(swiftBasic PRIVATE
  swiftDemangling
  ${UUID_LIBRARIES})

message(STATUS "Swift version: ${SWIFT_VERSION}")
message(STATUS "Swift vendor: ${SWIFT_VENDOR}")

string(REGEX REPLACE "([0-9]+)\\.[0-9]+(\\.[0-9]+)?" "\\1" SWIFT_VERSION_MAJOR
  ${SWIFT_VERSION})
string(REGEX REPLACE "[0-9]+\\.([0-9]+)(\\.[0-9]+)?" "\\1" SWIFT_VERSION_MINOR
  ${SWIFT_VERSION})

set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
  " -DSWIFT_VERSION=${SWIFT_VERSION} -DSWIFT_VERSION_MAJOR=${SWIFT_VERSION_MAJOR} -DSWIFT_VERSION_MINOR=${SWIFT_VERSION_MINOR}")

if ("${SWIFT_VERSION}" MATCHES "[0-9]+\\.[0-9]+\\.[0-9]+")
  string(REGEX REPLACE "[0-9]+\\.[0-9]+\\.([0-9]+)" "\\1" SWIFT_VERSION_PATCHLEVEL
    ${SWIFT_VERSION})
  set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
    " -DSWIFT_HAS_VERSION_PATCHLEVEL=1 -DSWIFT_VERSION_PATCHLEVEL=${SWIFT_VERSION_PATCHLEVEL}")
else()
  set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
    " -DSWIFT_HAS_VERSION_PATCHLEVEL=0")
endif()

if(NOT "${SWIFT_VENDOR}" STREQUAL "")
  set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
      " -DSWIFT_VENDOR=\"\\\"${SWIFT_VENDOR}\\\"\"")
endif()

set(SWIFT_COMPILER_VERSION "" CACHE STRING
    "The string that identifies the SCM commit(s) for this build")

message(STATUS "Swift compiler version: ${SWIFT_COMPILER_VERSION}")
message(STATUS "Embedded clang compiler version: ${CLANG_COMPILER_VERSION}")

if(SWIFT_COMPILER_VERSION)
  set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
    " -DSWIFT_COMPILER_VERSION=\"\\\"${SWIFT_COMPILER_VERSION}\\\"\"")
endif()

if(CLANG_COMPILER_VERSION)
  set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
    " -DCLANG_COMPILER_VERSION=\"\\\"${CLANG_COMPILER_VERSION}\\\"\"")
endif()

