//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if !$Embedded && (os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(visionOS))

import Swift

internal import Darwin

// .. Dynamic binding ..........................................................

enum CoreFoundation {
  static let path =
    "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation"

  static let handle = unsafe dlopen(path, RTLD_NOLOAD)

  static var isPresent: Bool { return unsafe handle != nil }

  static func symbol<T>(_ name: String) -> T {
    guard let result = unsafe dlsym(handle, name) else {
      fatalError("Unable to look up \(name) in CoreFoundation")
    }
    return unsafe unsafeBitCast(result, to: T.self)
  }

  static let CFRunLoopRun: @convention(c) () -> () =
    symbol("CFRunLoopRun")
  static let CFRunLoopGetMain: @convention(c) () -> OpaquePointer =
    unsafe symbol("CFRunLoopGetMain")
  static let CFRunLoopStop: @convention(c) (OpaquePointer) -> () =
    unsafe symbol("CFRunLoopStop")
}

// .. Main Executor ............................................................

@available(StdlibDeploymentTarget 6.2, *)
public final class CFMainExecutor: DispatchMainExecutor, @unchecked Sendable {

  override public func run() throws {
    CoreFoundation.CFRunLoopRun()
  }

  override public func stop() {
    unsafe CoreFoundation.CFRunLoopStop(CoreFoundation.CFRunLoopGetMain())
  }

}

// .. Task Executor ............................................................

@available(StdlibDeploymentTarget 6.2, *)
public final class CFTaskExecutor: DispatchGlobalTaskExecutor,
                                   @unchecked Sendable {

}

#endif
