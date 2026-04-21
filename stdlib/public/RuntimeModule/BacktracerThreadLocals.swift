//===--- BacktracerThreadLocals.swift - TLS for Swift Backtracer ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Holds thread-local storage for the Swift Backtracer
//
//===----------------------------------------------------------------------===//

import Swift

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
internal import Darwin
#elseif os(Windows)
internal import WinSDK
#elseif canImport(Glibc)
internal import Glibc
#elseif canImport(Musl)
internal import Musl
#endif

@available(BacktracingDT 6.2, *)
final class BacktracerThreadLocals {
  lazy var elfImageCache = ElfImageCache()
  lazy var peImageCache = PeImageCache()
  lazy var defaultSymbolLocator = DefaultSymbolLocator()

  #if os(Windows)
  private static var dwTlsIndex: DWORD = {
    let dwNdx = TlsAlloc()
    if dwNdx == TLS_OUT_OF_INDEXES {
      fatalError("Unable to allocate TSD for backtracer thread locals")
    }
    return dwNdx
  }()
  #else
  private static var key: pthread_key_t = {
    var theKey = pthread_key_t()
    let err = pthread_key_create(
      &theKey,
      { rawPtr in
        let ptr = Unmanaged<PeImageCache>.fromOpaque(
          notMutable(notOptional(rawPtr))
        )
        ptr.release()
      }
    )
    if err != 0 {
      fatalError("Unable to create TSD key for backtracer thread locals")
    }
    return theKey
  }()
  #endif

  static var threadLocal: BacktracerThreadLocals {
    #if os(Windows)
    guard let rawPtr = TlsGetValue(dwTlsIndex) else {
      let locals = Unmanaged<BacktracerThreadLocals>.passRetained(BacktracerThreadLocals())
      TlsSetValue(dwTlsIndex, locals.toOpaque())
      return locals.takeUnretainedValue()
    }
    #else
    guard let rawPtr = pthread_getspecific(key) else {
      let locals = Unmanaged<BacktracerThreadLocals>.passRetained(BacktracerThreadLocals())
      pthread_setspecific(key, locals.toOpaque())
      return locals.takeUnretainedValue()
    }
    #endif
    let locals = Unmanaged<BacktracerThreadLocals>.fromOpaque(rawPtr)
    return locals.takeUnretainedValue()
  }
}
