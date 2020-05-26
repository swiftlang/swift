add_swift_target_library(swiftOSLogTestHelper
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

  SWIFT_MODULE_DEPENDS_IOS Darwin ObjectiveC
  SWIFT_MODULE_DEPENDS_OSX Darwin ObjectiveC
  SWIFT_MODULE_DEPENDS_TVOS Darwin ObjectiveC
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin ObjectiveC
  TARGET_SDKS ALL_APPLE_PLATFORMS
  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT never_install
  DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}")
