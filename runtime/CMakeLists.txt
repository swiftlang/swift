# Concatenate all sources to form 'swift.swift' in the build
# directory.
set(SWIFTLIB_SOURCES
    Bool.swift
    File.swift
    FixedPoint.swift
    FloatingPoint.swift
    Misc.swift
    String.swift
    demangle.swift)

add_custom_command(
  OUTPUT ${CMAKE_BINARY_DIR}/lib/swift.swift
  COMMAND cat ${SWIFTLIB_SOURCES} > ${CMAKE_BINARY_DIR}/lib/swift.swift
  DEPENDS ${SWIFTLIB_SOURCES}
  COMMENT "FIXME: Concatenating Swift runtime sources into swift.swift"
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

# FIXME: We'd love to just enable the assembler support in CMake, but
# it doesn't work with the Xcode generator. Lame!
set(SHARED_LIBRARY ON)
add_swift_library(swift_runtime
  FastEntryPoints.s
  Alloc.cpp
  Stubs.cpp
  ObjCBridge.mm
  ${CMAKE_BINARY_DIR}/lib/swift.swift)

set_source_files_properties(${CMAKE_BINARY_DIR}/lib/swift.swift
  PROPERTIES GENERATED ON)

if(SWIFT_OPTIMIZED)
  if (CMAKE_SYSTEM_PROCESSOR STREQUAL "i386")
   set_target_properties(swift_runtime PROPERTIES
                                      COMPILE_FLAGS "-momit-leaf-frame-pointer")
  elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
   set_target_properties(swift_runtime PROPERTIES
                                      COMPILE_FLAGS "-momit-leaf-frame-pointer")
 endif()
endif()

# Link against Foundation, for ObjC bridging.
set_target_properties(swift_runtime PROPERTIES 
                                    LINK_FLAGS "-framework Foundation")
add_dependencies(swift_runtime swift)

# FIXME: Install swift.swift

if(XCODE)
  # Create the symlinks necessary to get the compiler to to find the
  # swift runtime during development with Xcode.
  add_custom_command(TARGET swift_runtime PRE_BUILD
    COMMAND mkdir "-p" "${CMAKE_BINARY_DIR}/lib/${CMAKE_CFG_INTDIR}"
    COMMAND ln "-fs" "${CMAKE_BINARY_DIR}/lib/${CMAKE_CFG_INTDIR}"
            "${CMAKE_BINARY_DIR}/bin/lib"
    COMMAND mkdir "-p" "${CMAKE_BINARY_DIR}/lib/${CMAKE_CFG_INTDIR}/swift"
    COMMAND ln "-fs" "${CMAKE_BINARY_DIR}/lib/swift.swift"
            "${CMAKE_BINARY_DIR}/lib/${CMAKE_CFG_INTDIR}/swift/swift.swift"
    COMMENT "Adding runtime symlinks for development builds")
endif()
