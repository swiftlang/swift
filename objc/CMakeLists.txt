if(XCODE)
  set(SWIFTLIB_DIR "${CMAKE_BINARY_DIR}/lib/${CMAKE_CFG_INTDIR}")
else()
  set(SWIFTLIB_DIR "${CMAKE_BINARY_DIR}/lib")
endif()

set(SHARED_LIBRARY ON)
add_swift_library(swiftFoundation
  Foundation.swift
  BridgeNSString.mm
  DEPENDS swift_stdlib)

if(SWIFT_OPTIMIZED)
  if (CMAKE_SYSTEM_PROCESSOR STREQUAL "i386")
   set_target_properties(swift_stdlib PROPERTIES
                                      COMPILE_FLAGS "-momit-leaf-frame-pointer")
  elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
   set_target_properties(swift_stdlib PROPERTIES
                                      COMPILE_FLAGS "-momit-leaf-frame-pointer")
 endif()
endif()

# Install objc.swift and Foundation.swift
if(XCODE)
  set(SWIFTLIB_DIR "${CMAKE_BINARY_DIR}/lib/${CMAKE_CFG_INTDIR}")
else()
  set(SWIFTLIB_DIR "${CMAKE_BINARY_DIR}/lib")
endif()

add_custom_target(swiftFoundationSources ALL
  COMMAND mkdir -p "${SWIFTLIB_DIR}/swift"
  COMMAND cp objc.swift "${SWIFTLIB_DIR}/swift/objc.swift"
  COMMAND cp Foundation.swift "${SWIFTLIB_DIR}/swift/Foundation.swift"
  DEPENDS objc.swift Foundation.swift
  COMMENT "Installing swiftFoundation sources"
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

# Link against Foundation
target_link_libraries(swiftFoundation
  "-framework Foundation"
  "-lSystem")
#set_target_properties(swiftFoundation 
#                      PROPERTIES 
#                      LINK_FLAGS "-framework Foundation" "-lSystem")
add_dependencies(swiftFoundation swift)
