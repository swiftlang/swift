add_swift_library(swiftGameplayKit IS_SDK_OVERLAY
  GameplayKit.swift
  GameplayKit.mm

  SWIFT_MODULE_DEPENDS Foundation simd
  FRAMEWORK_DEPENDS GameplayKit)
