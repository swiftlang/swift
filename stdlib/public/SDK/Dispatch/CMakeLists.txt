add_swift_library(swiftDispatch ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  Dispatch.swift
  Dispatch.mm
  Block.swift

  Data.swift
  IO.swift
  Private.swift
  Queue.swift
  Source.swift
  Time.swift
  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  SWIFT_MODULE_DEPENDS ObjectiveC)
