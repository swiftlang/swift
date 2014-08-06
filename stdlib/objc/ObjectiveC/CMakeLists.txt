set(SHARED_LIBRARY ON)

add_swift_library(swiftObjectiveC INSTALL TARGET_LIBRARY
  ObjectiveC.swift
  DEPENDS swiftCore swiftDarwin)
