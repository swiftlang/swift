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

public protocol RemoteProcess: AnyObject {
  associatedtype ProcessIdentifier
  associatedtype ProcessHandle

  var process: ProcessHandle { get }
  var context: SwiftReflectionContextRef! { get }

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
  public static var Free: FreeFunction? {
    return nil
  }
}

extension RemoteProcess {
  public func toOpaqueRef() -> UnsafeMutableRawPointer {
    return Unmanaged.passRetained(self).toOpaque()
  }

  public static func fromOpaque(_ ptr: UnsafeRawPointer) -> Self {
    return Unmanaged.fromOpaque(ptr).takeUnretainedValue()
  }

  public func release() {
    Unmanaged.passUnretained(self).release()
  }
}

public struct InspectOptions {
  public var nameOrPid: String = ""

#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
  public var forkCorpse: Bool = false
#endif

  public init() {}
}

public func inspect(options: InspectOptions,
                      _ body: (any RemoteProcess) throws -> Void) throws {
  guard let processId = process(matching: options.nameOrPid) else {
    print("No process found matching \(options.nameOrPid)")
    return
  }

#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
  guard let process = DarwinRemoteProcess(processId: processId,
                                          forkCorpse: options.forkCorpse) else {
    print("Failed to create inspector for process id \(processId)")
    return
  }
#elseif os(Windows)
  guard let process = WindowsRemoteProcess(processId: processId) else {
    print("Failed to create inspector for process id \(processId)")
    return
  }
#else
#error("Unsupported platform")
#endif

  try body(process)
}
