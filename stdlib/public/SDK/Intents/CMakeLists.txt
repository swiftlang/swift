add_swift_library(swiftIntents ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  Intents.swift
  INBooleanResolutionResult.swift
  INDoubleResolutionResult.swift
  INIntegerResolutionResult.swift
  INRequestRideIntent.swift
  INSaveProfileInCarIntent.swift
  INSearchForPhotosIntentResponse.swift
  INSetClimateSettingsInCarIntent.swift
  INSetDefrosterSettingsInCarIntent.swift
  INSetProfileInCarIntent.swift
  INSetRadioStationIntent.swift
  INSetSeatSettingsInCarIntent.swift
  INStartPhotoPlaybackIntentResponse.swift
  INStartWorkoutIntent.swift

  TARGET_SDKS OSX IOS IOS_SIMULATOR
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreGraphics CoreLocation Dispatch Foundation IOKit ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreLocation Dispatch Foundation ObjectiveC # auto-updated
  FRAMEWORK_DEPENDS_WEAK Intents
)
