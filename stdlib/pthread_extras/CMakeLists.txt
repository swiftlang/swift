add_swift_library(swiftSwiftUnstablePthreadExtras SHARED IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftUnstablePthreadExtras.swift
  PthreadBarriers.swift
  SwiftBlockToCFunctionThunks.cpp

  SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all
  INSTALL_IN_COMPONENT stdlib-experimental)

