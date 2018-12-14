cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_target_library(swiftCloudKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  NestedNames.swift
  TypealiasStrings.swift
  CKError.swift
  CKFetchRecordZoneChangesOperation.swift
  CKRecord.swift
  CKRecordZone_ID.swift
  CKRecord_ID.swift
  CKRecord_Reference.swift
  CKShare.swift
  CKShare_Participant.swift
  CKSubscription_NotificationInfo.swift
  CKRecordValue.swift
  DefaultParameters.swift

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}" "-swift-version" "4.2"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR WATCHOS WATCHOS_SIMULATOR

  SWIFT_MODULE_DEPENDS_OSX Darwin Contacts CoreFoundation CoreGraphics CoreLocation Dispatch Foundation IOKit ObjectiveC # auto-updated
    os XPC
  SWIFT_MODULE_DEPENDS_IOS Darwin Contacts CoreFoundation CoreLocation Dispatch Foundation ObjectiveC # auto-updated
    os
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreFoundation CoreLocation Dispatch Foundation ObjectiveC # auto-updated
    os
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin CoreFoundation CoreLocation Dispatch Foundation ObjectiveC # auto-updated
    os
  FRAMEWORK_DEPENDS_WEAK CloudKit

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_CLOUDKIT_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_CLOUDKIT_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_CLOUDKIT_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${SWIFTLIB_DEPLOYMENT_VERSION_CLOUDKIT_WATCHOS}
)
