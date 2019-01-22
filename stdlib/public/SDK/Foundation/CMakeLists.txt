cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_target_library(swiftFoundation ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  AffineTransform.swift
  Boxing.swift
  BundleLookup.mm
  Calendar.swift
  CharacterSet.swift
  Codable.swift
  Data.swift
  DataThunks.m
  Date.swift
  DateComponents.swift
  DateInterval.swift
  Decimal.swift
  FileManager.swift
  Foundation.swift
  IndexPath.swift
  IndexSet.swift
  JSONEncoder.swift
  Locale.swift
  Measurement.swift
  Notification.swift
  NSArray.swift
  NSCoder.swift
  NSDate.swift
  NSDictionary.swift
  NSError.mm
  NSError.swift
  NSExpression.swift
  NSFastEnumeration.swift
  NSGeometry.swift
  NSIndexSet.swift
  NSItemProvider.swift
  NSNumber.swift
  NSObject.swift
  NSPredicate.swift
  NSRange.swift
  NSSet.swift
  NSSortDescriptor.swift
  NSString.swift
  NSStringAPI.swift
  NSStringEncodings.swift
  NSTextCheckingResult.swift
  NSUndoManager.swift
  NSURL.swift
  NSValue.swift.gyb
  PersonNameComponents.swift
  PlistEncoder.swift
  Progress.swift
  ReferenceConvertible.swift
  String.swift
  TimeZone.swift
  URL.swift
  URLComponents.swift
  URLRequest.swift
  UUID.swift
  CheckClass.mm

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}" "-Xllvm" "-sil-inline-generics" "-Xllvm" "-sil-partial-specialization" "-swift-version" "5"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

  SWIFT_MODULE_DEPENDS_OSX Darwin CoreFoundation CoreGraphics Dispatch IOKit ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreFoundation CoreGraphics Dispatch ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreFoundation CoreGraphics Dispatch ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin CoreFoundation CoreGraphics Dispatch ObjectiveC # auto-updated
  FRAMEWORK_DEPENDS Foundation

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_FOUNDATION_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_FOUNDATION_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_FOUNDATION_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${SWIFTLIB_DEPLOYMENT_VERSION_FOUNDATION_WATCHOS}
)
