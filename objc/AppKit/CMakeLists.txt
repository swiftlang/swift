set(SHARED_LIBRARY ON)
add_swift_library(swiftAppKit INSTALL
  AppKit.swift
  DEPENDS swift_stdlib swiftObjectiveC swiftFoundation)

if(SWIFT_OPTIMIZED)
  if (CMAKE_SYSTEM_PROCESSOR STREQUAL "i386")
   set_target_properties(swiftAppKit PROPERTIES
                         COMPILE_FLAGS "-momit-leaf-frame-pointer")
  elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
   set_target_properties(swiftAppKit PROPERTIES
                         COMPILE_FLAGS "-momit-leaf-frame-pointer")
 endif()
endif()

# Link against frameworks
target_link_libraries(swiftAppKit
  "-framework AppKit")
