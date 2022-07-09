set(swift_oslog_darwin_dependencies "")
if (SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY)
    list(APPEND swift_oslog_darwin_dependencies "_Concurrency")
endif ()

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

  SWIFT_MODULE_DEPENDS_IOS Darwin ${swift_oslog_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_OSX Darwin ${swift_oslog_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_TVOS Darwin ${swift_oslog_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin ${swift_oslog_darwin_dependencies}
  TARGET_SDKS ALL_APPLE_PLATFORMS
  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT never_install
  DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}")
