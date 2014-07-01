set(SHARED_LIBRARY ON)

add_swift_library(swiftDispatch 
  Dispatch.swift
  Dispatch.mm
  INSTALL TARGET_LIBRARY
  DEPENDS swift_stdlib_core swiftObjectiveC)

add_swift_optimization_flags(swiftDispatch)
