set(SHARED_LIBRARY ON)
set(SWIFTXCTEST_SOURCES
    XCTest.swift
)

# XCTest.framework is provided by Xcode in the platforms, though that's recent
# and may not have made it to all platforms yet so fall back to the SDK in those
# cases.
#
# Also, since it's not in the default search paths, add a search path for it
# both when building and when linking. For compiling, that's accomplished by
# modifying SWIFT_EXTRA_FLAGS before invoking add_swift_library. For linking,
# that's accomplished by adding the extra flag to target_link_libraries since
# that will just pass through to the linker invocation.

if (EXISTS "${MODULES_SDK}/../../../Developer/Library/Frameworks")
  set(XCTEST_FRAMEWORK_LOCATION "${MODULES_SDK}/../../../Developer/Library/Frameworks")
else()
  set(XCTEST_FRAMEWORK_LOCATION "${MODULES_SDK}/Developer/Library/Frameworks")
endif()

list(APPEND SWIFT_EXTRA_FLAGS
  "-F${XCTEST_FRAMEWORK_LOCATION}"
  "-framework" "XCTest")

add_swift_library(swiftXCTest INSTALL
  ${SWIFTXCTEST_SOURCES}
  DEPENDS swift_stdlib_core swiftObjectiveC swiftFoundation)

add_swift_optimization_flags(swiftXCTest)

target_link_libraries(swiftXCTest
  "-F${XCTEST_FRAMEWORK_LOCATION}"
  "-framework Foundation" "-framework XCTest")
