set(SHARED_LIBRARY ON)
set(DISABLE_APPLICATION_EXTENSION ON)

# XCTest.framework is not in the default search paths, add a search path for it
# both when building and when linking. For compiling, that's accomplished by
# modifying SWIFT_EXTRA_FLAGS before invoking add_swift_library. For linking,
# that's accomplished by adding the extra flag to target_link_libraries since
# that will just pass through to the linker invocation.

set(XCTEST_FRAMEWORK_LOCATION "${MODULES_SDK}/../../../Developer/Library/Frameworks")

list(APPEND SWIFT_EXTRA_FLAGS
  "-F${XCTEST_FRAMEWORK_LOCATION}"
  "-framework" "XCTest")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -F${XCTEST_FRAMEWORK_LOCATION}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -framework XCTest")

add_swift_library(swiftXCTest INSTALL
  XCTestCaseAdditions.mm
  XCTest.swift
  DEPENDS swiftCore swiftObjectiveC swiftFoundation)

target_link_libraries(swiftXCTest
  "-F${XCTEST_FRAMEWORK_LOCATION}"
  "-framework Foundation" "-framework XCTest")
