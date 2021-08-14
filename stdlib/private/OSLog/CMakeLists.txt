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
  OSLogSwiftProtocols.swift
  OSLogPrivacy.swift
  OSLogFloatFormatting.swift

  SWIFT_MODULE_DEPENDS_IOS Darwin _Concurrency
  SWIFT_MODULE_DEPENDS_OSX Darwin _Concurrency
  SWIFT_MODULE_DEPENDS_TVOS Darwin _Concurrency
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin _Concurrency
  TARGET_SDKS ALL_APPLE_PLATFORMS
  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT never_install
  DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}")
