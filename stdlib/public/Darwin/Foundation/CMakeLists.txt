cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_target_library(swiftFoundation ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  AffineTransform.swift
  Boxing.swift
  BundleLookup.mm
  Calendar.swift
  CharacterSet.swift
  CheckClass.swift
  Codable.swift
  Collections+DataProtocol.swift
  CombineTypealiases.swift
  ContiguousBytes.swift
  Data.swift
  DataProtocol.swift
  DataThunks.m
  Date.swift
  DateComponents.swift
  DateInterval.swift
  Decimal.swift
  DispatchData+DataProtocol.swift
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
  NSData+DataProtocol.swift
  NSDate.swift
  NSDictionary.swift
  NSError.swift
  NSExpression.swift
  NSFastEnumeration.swift
  NSGeometry.swift
  NSIndexSet.swift
  NSItemProvider.swift
  NSNumber.swift
  NSObject.swift
  NSOrderedCollectionDifference.swift
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
  PersonNameComponents.swift
  PlistEncoder.swift
  Pointers+DataProtocol.swift
  Progress.swift
  Publishers+KeyValueObserving.swift
  Publishers+Locking.swift
  Publishers+NotificationCenter.swift
  Publishers+Timer.swift
  Publishers+URLSession.swift
  ReferenceConvertible.swift
  Scanner.swift
  Schedulers+Date.swift
  Schedulers+OperationQueue.swift
  Schedulers+RunLoop.swift
  String.swift
  TimeZone.swift
  URL.swift
  URLCache.swift
  URLComponents.swift
  URLRequest.swift
  URLSession.swift
  UUID.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  GYB_SOURCES
    NSValue.swift.gyb

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}" "-Xllvm" "-sil-inline-generics" "-Xllvm" "-sil-partial-specialization" "${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

  SWIFT_MODULE_DEPENDS_OSX Darwin CoreGraphics Dispatch CoreFoundation ObjectiveC # auto-updated
    CoreGraphics # imported in Swift
  SWIFT_MODULE_DEPENDS_IOS Darwin Dispatch CoreFoundation ObjectiveC # auto-updated
    CoreGraphics # imported in Swift
  SWIFT_MODULE_DEPENDS_TVOS Darwin Dispatch CoreFoundation ObjectiveC # auto-updated
    CoreGraphics # imported in Swift
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin Dispatch CoreFoundation ObjectiveC # auto-updated
    CoreGraphics # imported in Swift
  FRAMEWORK_DEPENDS Foundation
  FRAMEWORK_DEPENDS_WEAK Combine

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_FOUNDATION_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_FOUNDATION_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_FOUNDATION_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${SWIFTLIB_DEPLOYMENT_VERSION_FOUNDATION_WATCHOS}
  INSTALL_IN_COMPONENT sdk-overlay
)
