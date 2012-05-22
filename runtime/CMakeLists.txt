set(SHARED_LIBRARY ON)

add_swift_library(swift_runtime
  Alloc.cpp
  Stubs.cpp
  ObjCBridge.mm
  swift.swift)

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

# FIXME: Install swift.swift

if(XCODE)
  # Create the symlinks necessary to get the compiler to to find the
  # swift runtime during development with Xcode.
  add_custom_command(TARGET swift_runtime POST_BUILD
    COMMAND ln "-fs" "${CMAKE_BINARY_DIR}/lib/${CMAKE_CFG_INTDIR}"
            "${CMAKE_BINARY_DIR}/bin/lib"
    COMMAND mkdir "-p" "${CMAKE_BINARY_DIR}/lib/${CMAKE_CFG_INTDIR}/swift"
    COMMAND ln "-fs" "${CMAKE_CURRENT_SOURCE_DIR}/swift.swift"
            "${CMAKE_BINARY_DIR}/lib/${CMAKE_CFG_INTDIR}/swift/swift.swift"
    COMMENT "Adding runtime symlinks for development builds")
endif()
