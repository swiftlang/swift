cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_library(swiftIntents ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  INBooleanResolutionResult.swift
  INCallRecord.swift
  INDoubleResolutionResult.swift
  INGetCarLockStatusIntentResponse.swift
  INGetCarPowerLevelStatusIntentResponse.swift
  INIntegerResolutionResult.swift
  INIntent.swift
  INNotebookItemTypeResolutionResult.swift
  INParameter.swift
  INPlayMediaIntent.swift
  INRequestRideIntent.swift
  INRideOption.swift
  INSaveProfileInCarIntent.swift
  INSearchCallHistoryIntent.swift
  INSearchForPhotosIntentResponse.swift
  INSetCarLockStatusIntent.swift
  INSetClimateSettingsInCarIntent.swift
  INSetDefrosterSettingsInCarIntent.swift
  INSetProfileInCarIntent.swift
  INSetRadioStationIntent.swift
  INSetSeatSettingsInCarIntent.swift
  INShortcut.swift
  INStartPhotoPlaybackIntentResponse.swift
  INStartWorkoutIntent.swift
  NSStringIntents.swift

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS OSX IOS IOS_SIMULATOR WATCHOS WATCHOS_SIMULATOR
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreGraphics Dispatch IOKit Foundation CoreLocation CoreFoundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin Dispatch Foundation CoreLocation CoreFoundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin Dispatch Foundation CoreLocation CoreFoundation ObjectiveC # auto-updated
  FRAMEWORK_DEPENDS_WEAK Intents

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_INTENTS_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_INTENTS_IOS}
)
