add_swift_library(swiftStdlibUnittest SHARED IS_STDLIB
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
  TestHelpers.swift
  LifetimeTracked.swift

  # Can not serialize StdlibUnittest because of:
  # <rdar://problem/18917405> Compiling StdlibUnittest with -sil-serialize-all
  # crashes in SIL serializer
  #
  # SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all

  SWIFT_MODULE_DEPENDS Darwin Foundation
  FRAMEWORK_DEPENDS Foundation
  INSTALL_IN_COMPONENT stdlib-experimental)

