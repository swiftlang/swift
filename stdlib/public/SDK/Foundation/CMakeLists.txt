add_swift_library(swiftFoundation ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  Foundation.swift.gyb
  Boxing.swift
  NSError.swift
  NSStringAPI.swift
  NSValue.swift.gyb
  ExtraStringAPIs.swift
  ReferenceConvertible.swift
  AffineTransform.swift
  Calendar.swift
  TimeZone.swift
  Locale.swift
  CharacterSet.swift
  Date.swift
  DateComponents.swift
  DateInterval.swift
  Data.swift
  DataThunks.m
  Decimal.swift
  FileManager.swift
  FileManagerThunks.m
  IndexPath.swift
  IndexSet.swift
  IndexSetThunks.m
  Measurement.swift
  Notification.swift
  NSContiguousString.swift
  NSStringEncodings.swift
  PersonNameComponents.swift
  TypePreservingNSNumber.mm
  URL.swift
  URLComponents.swift
  URLRequest.swift
  UUID.swift
  Hashing.swift
  Hashing.m
  Thunks.mm

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

  SWIFT_MODULE_DEPENDS_OSX Darwin CoreGraphics Dispatch IOKit ObjectiveC # auto-updated
  # CoreGraphics is used by the overlay but not by the Foundation
  # framework, so we need to add it as a dependency.
  # The ./utils/find-overlay-dependencies.sh tool only touches the
  # OSX|IOS|TVOS|WATCHOS lines, so the standalone CoreGraphics lines remain.
  SWIFT_MODULE_DEPENDS_IOS Darwin Dispatch ObjectiveC # auto-updated
    CoreGraphics
  SWIFT_MODULE_DEPENDS_TVOS Darwin Dispatch ObjectiveC # auto-updated
    CoreGraphics
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin Dispatch ObjectiveC # auto-updated
    CoreGraphics
  FRAMEWORK_DEPENDS Foundation
)
