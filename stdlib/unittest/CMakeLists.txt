set(SHARED_LIBRARY ON)
set(SWIFTUNITTEST_SOURCES
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  StdlibUnittest.swift.gyb

  DarwinExtras.swift
  GetOSVersion.mm
  InterceptTraps.cpp
  OpaqueIdentityFunctions.cpp
  OpaqueIdentityFunctions.swift
  PthreadBarriers.swift
  PthreadWrappers.swift
  RaceTest.swift
  Statistics.swift
  StdlibCoreExtras.swift
  SwiftBlockToCFunctionThunks.cpp
  LifetimeTracked.swift
)

add_swift_library(swiftStdlibUnittest INSTALL TARGET_LIBRARY
  ${SWIFTUNITTEST_SOURCES}

  # Can not serialize StdlibUnittest because of:
  # <rdar://problem/18917405> Compiling StdlibUnittest with -sil-serialize-all
  # crashes in SIL serializer
  #
  # SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all

  FRAMEWORK_DEPENDS Foundation
  DEPENDS swiftCore swiftDarwin swiftFoundation)

