add_swift_library(swiftFoundation ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  Foundation.swift
  Boxing.swift
  NSError.swift
  NSStringAPI.swift
  NSValue.swift
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
  SWIFT_MODULE_DEPENDS ObjectiveC CoreGraphics Dispatch os
  SWIFT_MODULE_DEPENDS_OSX XPC
  FRAMEWORK_DEPENDS Foundation)

