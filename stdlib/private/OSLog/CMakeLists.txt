set(swift_oslog_darwin_dependencies "")
if(SWIFT_BUILD_SDK_OVERLAY)
    list(APPEND swift_oslog_darwin_dependencies "Darwin")
endif()
if (SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY)
    list(APPEND swift_oslog_darwin_dependencies "_Concurrency")
endif()
if (SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING)
  list(APPEND swift_oslog_darwin_dependencies "_StringProcessing")
endif()

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

  SWIFT_MODULE_DEPENDS_IOS ${swift_oslog_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_OSX ${swift_oslog_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_TVOS ${swift_oslog_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_WATCHOS ${swift_oslog_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_XROS ${swift_oslog_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_MACCATALYST ${swift_oslog_darwin_dependencies}
  TARGET_SDKS ALL_APPLE_PLATFORMS
  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT never_install
  DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}"
  MACCATALYST_BUILD_FLAVOR "zippered")
