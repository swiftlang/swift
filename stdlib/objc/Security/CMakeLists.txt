set(SHARED_LIBRARY ON)

add_swift_library(swiftSecurity
  Security.swift
  INSTALL TARGET_LIBRARY
  FRAMEWORK_DEPENDS Security
  DEPENDS swiftCore swiftObjectiveC swiftDispatch swiftDarwin)

