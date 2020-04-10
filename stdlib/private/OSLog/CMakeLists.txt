foreach(sdk ${SWIFT_SDKS})
if(NOT ${sdk} IN_LIST SWIFT_APPLE_PLATFORMS)
  continue()
endif()

add_swift_target_library_single_sdk(swiftOSLogTestHelper
  IS_SDK_OVERLAY
  SHARED

  OSLogTestHelper.swift
  OSLogMessage.swift
  OSLogIntegerFormatting.swift
  OSLogStringAlignment.swift
  OSLogIntegerTypes.swift
  OSLogStringTypes.swift
  OSLogNSObjectType.swift
  OSLogFloatingPointTypes.swift

  SWIFT_MODULE_DEPENDS Darwin ObjectiveC
  SDK ${sdk}
  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT never_install
  DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}")
endforeach()
