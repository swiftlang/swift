add_swift_library(swiftCoreBluetooth IS_SDK_OVERLAY
  CoreBluetooth.swift
  SWIFT_MODULE_DEPENDS Foundation
  FRAMEWORK_DEPENDS CoreBluetooth)
