add_swift_library(swiftFoundation ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  AffineTransform.swift
  Boxing.swift
  Calendar.swift
  CharacterSet.swift
  Data.swift
  DataThunks.m
  Date.swift
  DateComponents.swift
  DateInterval.swift
  Decimal.swift
  ExtraStringAPIs.swift
  FileManager.swift
  FileManagerThunks.m
  Foundation.swift
  Hashing.m
  Hashing.swift
  IndexPath.swift
  IndexSet.swift
  IndexSetThunks.m
  Locale.swift
  Measurement.swift
  Notification.swift
  NSArray.swift
  NSCoder.swift
  NSDate.swift
  NSDictionary.swift
  NSError.swift
  NSExpression.swift
  NSFastEnumeration.swift
  NSGeometry.swift
  NSIndexSet.swift
  NSNumber.swift.gyb
  NSPredicate.swift
  NSRange.swift
  NSSet.swift
  NSString.swift
  NSStringAPI.swift
  NSStringEncodings.swift
  NSTextCheckingResult.swift
  NSUndoManager.swift
  NSURL.swift
  NSValue.swift.gyb
  PersonNameComponents.swift
  ReferenceConvertible.swift
  String.swift
  Thunks.mm
  TimeZone.swift
  TypePreservingNSNumber.mm
  URL.swift
  URLComponents.swift
  URLRequest.swift
  UUID.swift

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

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_FOUNDATION_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_FOUNDATION_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_FOUNDATION_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${SWIFTLIB_DEPLOYMENT_VERSION_FOUNDATION_WATCHOS}
)
