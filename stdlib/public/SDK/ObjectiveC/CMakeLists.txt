add_swift_library(swiftObjectiveC IS_SDK_OVERLAY
  ObjectiveC.mm
  ObjectiveC.swift
  SWIFT_MODULE_DEPENDS Darwin)

