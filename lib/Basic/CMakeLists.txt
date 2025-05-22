
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

function(generate_revision_inc revision_inc_var name dir)
  if(SWIFT_APPEND_VC_REV)
    # generate_vcs_version_script generates header with only `undef`s
    # inside when source directory doesn't exist.
    find_first_existing_vc_file("${dir}" ${name}_vc)
    set(dir_when_append_enabled ${dir})
  endif()

  # Create custom target to generate the VC revision include.
  set(version_inc "${CMAKE_CURRENT_BINARY_DIR}/${name}Revision.inc")

  set(generate_vcs_version_script "${LLVM_MAIN_SRC_DIR}/cmake/modules/GenerateVersionFromVCS.cmake")

  add_custom_command(OUTPUT "${version_inc}"
    DEPENDS "${${name}_vc}" "${generate_vcs_version_script}"
    COMMAND ${CMAKE_COMMAND} "-DNAMES=$<UPPER_CASE:${name}>"
                             "-D$<UPPER_CASE:${name}>_SOURCE_DIR=${dir_when_append_enabled}"
                             "-DHEADER_FILE=${version_inc}"
                             -P "${generate_vcs_version_script}")

  # Mark the generated header as being generated.
  set_source_files_properties("${version_inc}"
    PROPERTIES GENERATED TRUE
               HEADER_FILE_ONLY TRUE)
  set(${revision_inc_var} ${version_inc} PARENT_SCOPE)
endfunction()

generate_revision_inc(llvm_revision_inc LLVM "${LLVM_MAIN_SRC_DIR}")
generate_revision_inc(swift_revision_inc Swift "${SWIFT_SOURCE_DIR}")

add_swift_host_library(swiftBasic STATIC
  Assertions.cpp
  BasicBridging.cpp
  BasicSourceInfo.cpp
  Cache.cpp
  CASOptions.cpp
  ClusteredBitVector.cpp
  DiverseStack.cpp
  Edit.cpp
  EditorPlaceholder.cpp
  ExponentialGrowthAppendingBinaryByteStream.cpp
  Feature.cpp
  FileSystem.cpp
  FileTypes.cpp
  Fingerprint.cpp
  ParseableOutput.cpp
  JSONSerialization.cpp
  LangOptions.cpp
  LoadDynamicLibrary.cpp
  Located.cpp
  Mangler.cpp
  OutputFileMap.cpp
  Platform.cpp
  PrefixMap.cpp
  PrettyStackTrace.cpp
  PrimitiveParsing.cpp
  Program.cpp
  QuotedString.cpp
  Sandbox.cpp
  SmallBitVector.cpp
  SourceLoc.cpp
  StableHasher.cpp
  Statistic.cpp
  StringExtras.cpp
  SupportedFeatures.cpp
  TargetInfo.cpp
  TaskQueue.cpp
  ThreadSafeRefCounted.cpp
  Unicode.cpp
  UUID.cpp
  Version.cpp
  BlockList.cpp

  ${llvm_revision_inc}
  ${clang_revision_inc}
  ${swift_revision_inc}

  # Platform-specific TaskQueue implementations
  Unix/TaskQueue.inc

  # Platform-agnostic fallback TaskQueue implementation
  Default/TaskQueue.inc

  LLVM_LINK_COMPONENTS support targetparser)
_swift_gyb_target_sources(swiftBasic PRIVATE
    UnicodeExtendedGraphemeClusters.cpp.gyb)

target_include_directories(swiftBasic PRIVATE
  clangBasic
  ${UUID_INCLUDE})

target_link_libraries(swiftBasic PUBLIC
  swiftDemangling)
target_link_libraries(swiftBasic PRIVATE
  ${UUID_LIBRARIES})

include(${CMAKE_CURRENT_LIST_DIR}/../../cmake/SwiftVersion.cmake)
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

message(STATUS "Swift compiler version: ${SWIFT_COMPILER_VERSION}")
message(STATUS "Swift toolchain version: ${SWIFT_TOOLCHAIN_VERSION}")
message(STATUS "Embedded clang compiler version: ${CLANG_COMPILER_VERSION}")

if(SWIFT_COMPILER_VERSION)
  set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
    " -DSWIFT_COMPILER_VERSION=\"\\\"${SWIFT_COMPILER_VERSION}\\\"\"")
endif()

if(CLANG_COMPILER_VERSION)
  set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
    " -DCLANG_COMPILER_VERSION=\"\\\"${CLANG_COMPILER_VERSION}\\\"\"")
endif()

if(SWIFT_TOOLCHAIN_VERSION)
  set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
    " -DSWIFT_TOOLCHAIN_VERSION=\"\\\"${SWIFT_TOOLCHAIN_VERSION}\\\"\"")
endif()
