add_swift_library(swiftFoundation ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  Foundation.swift
  Boxing.swift
  NSError.swift
  NSStringAPI.swift
  NSValue.swift
  ExtraStringAPIs.swift
  ReferenceConvertible.swift
  AffineTransform.swift
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
  NotificationThunks.m
  NSStringEncodings.swift
  PersonNameComponents.swift
  URL.swift
  URLComponents.swift
  URLRequest.swift
  UUID.swift
  UserInfo.swift
  Hashing.swift
  Hashing.m
  Thunks.mm

  SWIFT_MODULE_DEPENDS ObjectiveC CoreGraphics Dispatch
  SWIFT_MODULE_DEPENDS_OSX XPC
  FRAMEWORK_DEPENDS Foundation)

