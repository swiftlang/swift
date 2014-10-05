# Re-enable this if you add assembly files to this target again.
# if (XCODE)
#   # Assembly files with Xcode generator require cmake 2.8.10.
#   cmake_minimum_required(VERSION 2.8.10)
#   set(CMAKE_ASM_COMPILER_ID "Clang")
#   enable_language(ASM)
# else ()
#   # FIXME: CMake assembler rules don't pass things 
#   # like architecture flags with the makefile generator.
#   SET_SOURCE_FILES_PROPERTIES(FastEntryPoints.s PROPERTIES LANGUAGE C)
# endif()

if (SWIFT_OPTIMIZED)
  set(SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS_DEFAULT OFF)
else()
  set(SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS_DEFAULT ON)
endif()

set(SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS
  ${SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS_DEFAULT}
  CACHE BOOL "Overwrite memory for deallocated Swift objects")

set(SWIFT_RUNTIME_COMPILE_FLAGS)
if(SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS)
  list(APPEND SWIFT_RUNTIME_COMPILE_FLAGS "-DSWIFT_RUNTIME_CLOBBER_FREED_OBJECTS=1")
endif()

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(SWIFT_RUNTIME_OBJC_SOURCES
    Availability.mm
    SwiftObject.mm
    NSSwiftXXXBase.mm.gyb
    Reflection.mm)

  set(FRAMEWORKS Foundation)
else()
  set(SWIFT_RUNTIME_OBJC_SOURCES)
  set(FRAMEWORKS)
endif()

add_swift_library(swiftRuntime
  Casting.cpp
  Demangle.cpp
  GlobalObjects.cpp
  HeapObject.cpp
  KnownMetadata.cpp
  Metadata.cpp
  Stubs.cpp
  Enum.cpp
  Once.cpp
  Heap.cpp
  Errors.cpp
  UnicodeExtendedGraphemeClusters.cpp.gyb
  ${SWIFT_RUNTIME_OBJC_SOURCES}
  COMPILE_FLAGS ${SWIFT_RUNTIME_COMPILE_FLAGS}
  TARGET_LIBRARY INSTALL
  FRAMEWORK_DEPENDS ${FRAMEWORKS})


list(APPEND CMAKE_FIND_ROOT_PATH "${CMAKE_OSX_SYSROOT}")
find_library(CRASHREPORTERCLIENT CrashReporterClient ONLY_CMAKE_FIND_ROOT_PATH)

if (CRASHREPORTERCLIENT)
  target_link_libraries(swiftRuntime "-lCrashReporterClient")
  set_target_properties(swiftRuntime 
    PROPERTIES COMPILE_FLAGS "-DSWIFT_HAVE_CRASHREPORTERCLIENT=1")
endif()
