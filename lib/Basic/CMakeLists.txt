find_package(UUID REQUIRED)

add_swift_library(swiftBasic
  Cache.cpp
  ClusteredBitVector.cpp
  Demangle.cpp
  DemangleWrappers.cpp
  DiagnosticConsumer.cpp
  DiverseStack.cpp
  JSONSerialization.cpp
  LangOptions.cpp
  Platform.cpp
  PrettyStackTrace.cpp
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

  C_COMPILE_FLAGS "-I${UUID_INCLUDE_DIRS}"
  LINK_LIBRARIES ${UUID_LIBRARIES}
  COMPONENT_DEPENDS support)

set(SWIFT_VERSION "1.1")
message(STATUS "Swift version: ${SWIFT_VERSION}")

string(REGEX REPLACE "([0-9]+)\\.[0-9]+(\\.[0-9]+)?" "\\1" SWIFT_VERSION_MAJOR
  ${SWIFT_VERSION})
string(REGEX REPLACE "[0-9]+\\.([0-9]+)(\\.[0-9]+)?" "\\1" SWIFT_VERSION_MINOR
  ${SWIFT_VERSION})

set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
  " -DSWIFT_VERSION=${SWIFT_VERSION} -DSWIFT_VERSION_MAJOR=${SWIFT_VERSION_MAJOR} -DSWIFT_VERSION_MINOR=${SWIFT_VERSION_MINOR}")

# Default swift submit version quad for local builds
set(SWIFT_VERSION_QUAD "600.0.20.0")

if (LLVM_SUBMIT_VERSION)
  set(SWIFT_VERSION_QUAD "${LLVM_SUBMIT_VERSION}.${LLVM_SUBMIT_SUBVERSION}")
endif()

set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
  " -DSWIFT_SUBMIT_VERSION_QUAD_STRING=\"${SWIFT_VERSION_QUAD}\"")

set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
  " -DSWIFT_SUBMIT_VERSION_STRING=\"swift-${SWIFT_VERSION_QUAD}\"")

if (${SWIFT_VERSION} MATCHES "[0-9]+\\.[0-9]+\\.[0-9]+")
  string(REGEX REPLACE "[0-9]+\\.[0-9]+\\.([0-9]+)" "\\1" SWIFT_VERSION_PATCHLEVEL
    ${SWIFT_VERSION})
  set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
    " -DSWIFT_HAS_VERSION_PATCHLEVEL=1 -DSWIFT_VERSION_PATCHLEVEL=${SWIFT_VERSION_PATCHLEVEL}")
else()
  set_property(SOURCE Version.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
    " -DSWIFT_HAS_VERSION_PATCHLEVEL=0")
endif()

