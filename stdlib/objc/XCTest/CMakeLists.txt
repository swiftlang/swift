set(DISABLE_APPLICATION_EXTENSION ON)

add_swift_library(swiftXCTest IS_SDK_OVERLAY
  XCTestCaseAdditions.mm
  XCTest.swift
  SWIFT_MODULE_DEPENDS ObjectiveC Foundation
  FRAMEWORK_DEPENDS Foundation XCTest)

