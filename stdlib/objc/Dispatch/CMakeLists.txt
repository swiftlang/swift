set(SHARED_LIBRARY ON)

add_swift_library(swiftDispatch 
  Dispatch.swift
  Dispatch.mm
  INSTALL TARGET_LIBRARY
  DEPENDS swiftCore swiftObjectiveC)
