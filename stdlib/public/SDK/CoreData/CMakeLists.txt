add_swift_library(swiftCoreData IS_SDK_OVERLAY
  CoreData.swift
  CoreData.mm

  SWIFT_MODULE_DEPENDS Foundation
  FRAMEWORK_DEPENDS CoreData)

