add_swift_library(swiftDispatch IS_SDK_OVERLAY
  Dispatch.swift
  Dispatch.mm
  Block.swift
  Data.swift
  IO.swift
  Private.swift
  Queue.swift
  Source.swift
  Time.swift
  SWIFT_MODULE_DEPENDS ObjectiveC)
