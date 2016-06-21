add_swift_library(swiftObjectiveC ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  ObjectiveC.mm
  ObjectiveC.swift
  SWIFT_MODULE_DEPENDS Darwin)

