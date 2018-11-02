//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import os
@_exported import os.log
import _SwiftOSOverlayShims

@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public func os_log(
  _ type: OSLogType,
  dso: UnsafeRawPointer = #dsohandle,
  log: OSLog = .default,
  _ message: StaticString,
  _ args: CVarArg...)
{
  guard log.isEnabled(type: type) else { return }
  let ra = _swift_os_log_return_address()

  message.withUTF8Buffer { (buf: UnsafeBufferPointer<UInt8>) in
    // Since dladdr is in libc, it is safe to unsafeBitCast
    // the cstring argument type.
    buf.baseAddress!.withMemoryRebound(
      to: CChar.self, capacity: buf.count
    ) { str in
      withVaList(args) { valist in
        _swift_os_log(dso, ra, log, type, str, valist)
      }
    }
  }
}

@available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
public func os_log(
  _ message: StaticString,
  dso: UnsafeRawPointer? = #dsohandle,
  log: OSLog = .default,
  type: OSLogType = .default,
  _ args: CVarArg...)
{
  guard log.isEnabled(type: type) else { return }
  let ra = _swift_os_log_return_address()

  message.withUTF8Buffer { (buf: UnsafeBufferPointer<UInt8>) in
    // Since dladdr is in libc, it is safe to unsafeBitCast
    // the cstring argument type.
    buf.baseAddress!.withMemoryRebound(
      to: CChar.self, capacity: buf.count
    ) { str in
      withVaList(args) { valist in
        _swift_os_log(dso, ra, log, type, str, valist)
      }
    }
  }
}

extension OSLogType {
  @available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
  public static let `default` = __OS_LOG_TYPE_DEFAULT

  @available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
  public static let info = __OS_LOG_TYPE_INFO

  @available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
  public static let debug = __OS_LOG_TYPE_DEBUG

  @available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
  public static let error = __OS_LOG_TYPE_ERROR

  @available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
  public static let fault = __OS_LOG_TYPE_FAULT
}

extension OSLog {
  @available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
  public static let disabled = _swift_os_log_disabled()

  @available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
  public static let `default` = _swift_os_log_default()

  @available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
  public convenience init(subsystem: String, category: String) {
    self.init(__subsystem: subsystem, category: category)
  }
}

@available(*, unavailable, renamed: "OSLogType.default")
public var OS_LOG_TYPE_DEFAULT: OSLogType {
  fatalError()
}

@available(*, unavailable, renamed: "OSLogType.info")
public var OS_LOG_TYPE_INFO: OSLogType {
  fatalError()
}

@available(*, unavailable, renamed: "OSLogType.debug")
public var OS_LOG_TYPE_DEBUG: OSLogType {
  fatalError()
}

@available(*, unavailable, renamed: "OSLogType.error")
public var OS_LOG_TYPE_ERROR: OSLogType {
  fatalError()
}

@available(*, unavailable, renamed: "OSLogType.fault")
public var OS_LOG_TYPE_FAULT: OSLogType {
  fatalError()
}
