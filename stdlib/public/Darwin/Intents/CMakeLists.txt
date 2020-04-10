cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

foreach(sdk ${SWIFT_SDKS})

if(${sdk} STREQUAL OSX
  OR ${sdk} STREQUAL IOS OR ${sdk} STREQUAL IOS_SIMULATOR
  OR ${sdk} STREQUAL WATCHOS OR ${sdk} STREQUAL WATCHOS_SIMULATOR)
else()
  continue()
endif()

select_swift_overlay_dependenies(module_depends
  SDK ${sdk}
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreGraphics Dispatch IOKit Foundation CoreLocation XPC CoreFoundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin Dispatch Foundation CoreLocation CoreFoundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin Dispatch Foundation CoreLocation CoreFoundation ObjectiveC # auto-updated
)

add_swift_target_library_single_sdk(swiftIntents ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
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

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  SDK ${sdk}
  SWIFT_MODULE_DEPENDS ${module_depends}
  FRAMEWORK_DEPENDS_WEAK Intents

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_INTENTS_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_INTENTS_IOS}
  INSTALL_IN_COMPONENT sdk-overlay
)

endforeach()
