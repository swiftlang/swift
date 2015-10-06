
# On non-Darwin require UUID.
if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set(UUID_INCLUDE "")
  set(UUID_LIBRARIES "")
else()
  find_package(UUID REQUIRED)
  set(UUID_INCLUDE "-I${UUID_INCLUDE_DIRS}")
endif()

add_swift_library(swiftBasic
  Cache.cpp
  ClusteredBitVector.cpp
  Demangle.cpp
  DemangleWrappers.cpp
  DiagnosticConsumer.cpp
  DiverseStack.cpp
  EditorPlaceholder.cpp
  FileSystem.cpp
  JSONSerialization.cpp
  LangOptions.cpp
  Platform.cpp
  PrettyStackTrace.cpp
  PrefixMap.cpp
  PrimitiveParsing.cpp
  Program.cpp
  Punycode.cpp
  PunycodeUTF8.cpp
  QuotedString.cpp
  Remangle.cpp
  SourceLoc.cpp
  StringExtras.cpp
  TaskQueue.cpp
  ThreadSafeRefCounted.cpp
  Unicode.cpp
  UUID.cpp
  Version.cpp

  # Platform-specific TaskQueue implementations
  Unix/TaskQueue.inc

  # Platform-agnostic fallback TaskQueue implementation
  Default/TaskQueue.inc

  UnicodeExtendedGraphemeClusters.cpp.gyb

  C_COMPILE_FLAGS ${UUID_INCLUDE}
  LINK_LIBRARIES ${UUID_LIBRARIES}
  COMPONENT_DEPENDS support)

message(STATUS "Swift version: ${SWIFT_VERSION}")
message(STATUS "Swift vendor: ${SWIFT_VENDOR}")

string(REGEX REPLACE "([0-9]+)\\.[0-9]+(\\.[0-9]+)?" "\\1" SWIFT_VERSION_MAJOR
  ${SWIFT_VERSION})
string(REGEX REPLACE "[0-9]+\\.([0-9]+)(\\.[0-9]+)?" "\\1" SWIFT_VERSION_MINOR
  ${SWIFT_VERSION})

set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
  " -DSWIFT_VERSION=${SWIFT_VERSION} -DSWIFT_VERSION_MAJOR=${SWIFT_VERSION_MAJOR} -DSWIFT_VERSION_MINOR=${SWIFT_VERSION_MINOR} -DSWIFT_COMPILER_VERSION=${SWIFT_COMPILER_VERSION}")

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

if(NOT "${CLANG_REPOSITORY_STRING}" STREQUAL "")
  set(SWIFT_COMPILER_VERSION
      "${SWIFT_COMPILER_VERSION} ${CLANG_REPOSITORY_STRING}")
endif()

message(STATUS "Swift repository string: ${SWIFT_COMPILER_VERSION}")

if(NOT "${SWIFT_COMPILER_VERSION}" STREQUAL "")
  set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
    " -DSWIFT_COMPILER_VERSION=\"\\\"${SWIFT_COMPILER_VERSION}\\\"\"")
endif()

