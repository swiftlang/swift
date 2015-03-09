add_swift_library(swiftSecurity IS_SDK_OVERLAY
  Security.swift
  SWIFT_MODULE_DEPENDS ObjectiveC Dispatch Darwin
  FRAMEWORK_DEPENDS Security)

