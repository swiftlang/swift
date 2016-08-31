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

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS OSX IOS IOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation CoreLocation
  SWIFT_MODULE_DEPENDS_OSX AppKit
  SWIFT_MODULE_DEPENDS_IOS UIKit
  FRAMEWORK_DEPENDS_WEAK Intents)
