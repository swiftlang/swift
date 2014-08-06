set(SHARED_LIBRARY ON)
set(SWIFTUNITTEST_SOURCES
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  StdlibUnittest.swift.gyb

  GetOSVersion.mm
  InterceptTraps.cpp
  PthreadBarriers.swift
  PthreadWrappers.swift
  SwiftBlockToCFunctionThunks.cpp
)

add_swift_library(swiftStdlibUnittest INSTALL
  ${SWIFTUNITTEST_SOURCES}
  FRAMEWORK_DEPENDS Foundation
  DEPENDS swiftCore swiftDarwin)

