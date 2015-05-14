add_swift_library(swiftContacts IS_SDK_OVERLAY
  Contacts.swift
  TARGET_SDKS OSX IOS IOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation)
