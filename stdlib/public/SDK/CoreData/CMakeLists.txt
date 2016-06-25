add_swift_library(swiftCoreData ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  CoreData.swift
  CoreData.mm

  SWIFT_MODULE_DEPENDS Foundation
  FRAMEWORK_DEPENDS CoreData)

