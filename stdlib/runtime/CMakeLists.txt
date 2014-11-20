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

if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set(SWIFT_RUNTIME_OBJC_SOURCES
    Availability.mm
    SwiftObject.mm
    SwiftNativeNSXXXBase.mm.gyb
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
  Reflection.cpp
  Stubs.cpp
  SwiftObject.cpp
  Enum.cpp
  Once.cpp
  Heap.cpp
  Errors.cpp
  UnicodeExtendedGraphemeClusters.cpp.gyb
  ${SWIFT_RUNTIME_OBJC_SOURCES}
  C_COMPILE_FLAGS ${SWIFT_RUNTIME_COMPILE_FLAGS}
  TARGET_LIBRARY INSTALL
  FRAMEWORK_DEPENDS ${FRAMEWORKS})

if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  # FIXME: This is probably not flexible enough to deal with 32bit builds,
  # but SWIFTLIB_DIR isn't available in this cmake file and duplicating
  # the logic seems wrong. rdar://problem/19035586
  configure_file(swift.ld ${CMAKE_BINARY_DIR}/lib/swift/swift.ld COPYONLY)
  target_link_libraries(swiftRuntime "-lpthread" "-ldl")
endif()

list(APPEND CMAKE_FIND_ROOT_PATH "${CMAKE_OSX_SYSROOT}")
find_library(CRASHREPORTERCLIENT CrashReporterClient ONLY_CMAKE_FIND_ROOT_PATH)

if (CRASHREPORTERCLIENT)
  target_link_libraries(swiftRuntime "-lCrashReporterClient")
  set_property(TARGET swiftRuntime APPEND_STRING PROPERTY
    COMPILE_FLAGS " -DSWIFT_HAVE_CRASHREPORTERCLIENT=1")
endif()
