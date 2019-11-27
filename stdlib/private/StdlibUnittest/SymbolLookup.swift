//===--- SymbolLookup.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
  import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android) || os(Cygwin) || os(Haiku)
  import Glibc
#elseif os(Windows)
  import MSVCRT
  import WinSDK
#else
#error("Unsupported platform")
#endif

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
  let RTLD_DEFAULT = UnsafeMutableRawPointer(bitPattern: -2)
#elseif os(Linux)
  let RTLD_DEFAULT = UnsafeMutableRawPointer(bitPattern: 0)
#elseif os(Android)
  #if arch(arm) || arch(i386)
    let RTLD_DEFAULT = UnsafeMutableRawPointer(bitPattern: 0xffffffff as UInt)
  #elseif arch(arm64) || arch(x86_64)
    let RTLD_DEFAULT = UnsafeMutableRawPointer(bitPattern: 0)
  #else
    #error("Unsupported platform")
  #endif
#elseif os(Windows)
  let hStdlibCore: HMODULE = GetModuleHandleA("swiftCore.dll")!
#else
  #error("Unsupported platform")
#endif

public func pointerToSwiftCoreSymbol(name: String) -> UnsafeMutableRawPointer? {
#if os(Windows)
  return unsafeBitCast(GetProcAddress(hStdlibCore, name),
                       to: UnsafeMutableRawPointer?.self)
#else
  return dlsym(RTLD_DEFAULT, name)
#endif
}
