set(SHARED_LIBRARY ON)
add_swift_library(swiftFoundation INSTALL
  Foundation.swift
  BridgeNSString.mm
  DEPENDS swift_stdlib_core swiftObjectiveC)

if(SWIFT_OPTIMIZED)
  if (CMAKE_SYSTEM_PROCESSOR STREQUAL "i386")
   set_target_properties(swiftFoundation PROPERTIES
                         COMPILE_FLAGS "-momit-leaf-frame-pointer")
  elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
   set_target_properties(swiftFoundation PROPERTIES
                         COMPILE_FLAGS "-momit-leaf-frame-pointer")
 endif()
endif()

# Link against frameworks
target_link_libraries(swiftFoundation
  "-framework Foundation")
