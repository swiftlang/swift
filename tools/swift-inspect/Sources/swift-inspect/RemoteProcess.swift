//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftRemoteMirror

internal protocol RemoteProcess: AnyObject {
  associatedtype ProcessIdentifier
  associatedtype ProcessHandle

  var process: ProcessHandle { get }
  var context: SwiftReflectionContextRef! { get }
  var processIdentifier: ProcessIdentifier { get }
  var processName: String { get }

  typealias QueryDataLayoutFunction =
      @convention(c) (UnsafeMutableRawPointer?, DataLayoutQueryType,
                      UnsafeMutableRawPointer?, UnsafeMutableRawPointer?) -> CInt
  typealias FreeFunction =
      @convention(c) (UnsafeMutableRawPointer?, UnsafeRawPointer?,
                      UnsafeMutableRawPointer?) -> Void
  typealias ReadBytesFunction =
      @convention(c) (UnsafeMutableRawPointer?, swift_addr_t, UInt64,
                      UnsafeMutablePointer<UnsafeMutableRawPointer?>?) -> UnsafeRawPointer?
  typealias GetStringLengthFunction =
      @convention(c) (UnsafeMutableRawPointer?, swift_addr_t) -> UInt64
  typealias GetSymbolAddressFunction =
      @convention(c) (UnsafeMutableRawPointer?, UnsafePointer<CChar>?, UInt64) -> swift_addr_t

  static var QueryDataLayout: QueryDataLayoutFunction { get }
  static var Free: FreeFunction? { get }
  static var ReadBytes: ReadBytesFunction { get }
  static var GetStringLength: GetStringLengthFunction { get }
  static var GetSymbolAddress: GetSymbolAddressFunction { get }

  func symbolicate(_ address: swift_addr_t) -> (module: String?, symbol: String?)
  func iterateHeap(_ body: (swift_addr_t, UInt64) -> Void)
}

extension RemoteProcess {
  internal func toOpaqueRef() -> UnsafeMutableRawPointer {
    return Unmanaged.passRetained(self).toOpaque()
  }

  internal static func fromOpaque(_ ptr: UnsafeRawPointer) -> Self {
    return Unmanaged.fromOpaque(ptr).takeUnretainedValue()
  }

  internal func release() {
    Unmanaged.passUnretained(self).release()
  }
}
