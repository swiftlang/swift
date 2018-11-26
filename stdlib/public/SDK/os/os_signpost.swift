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
@_exported import os.signpost
import _SwiftOSOverlayShims
import os.log

@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public func os_signpost(
  _ type: OSSignpostType,
  dso: UnsafeRawPointer = #dsohandle,
  log: OSLog,
  name: StaticString,
  signpostID: OSSignpostID = .exclusive
) {
  let hasValidID = signpostID != .invalid && signpostID != .null
  guard log.signpostsEnabled && hasValidID else { return }
  let ra = _swift_os_log_return_address()
  name.withUTF8Buffer { (nameBuf: UnsafeBufferPointer<UInt8>) in
    // Since dladdr is in libc, it is safe to unsafeBitCast
    // the cstring argument type.
    nameBuf.baseAddress!.withMemoryRebound(
      to: CChar.self, capacity: nameBuf.count
    ) { nameStr in
      _swift_os_signpost(dso, ra, log, type, nameStr, signpostID.rawValue)
    }
  }
}

@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public func os_signpost(
  _ type: OSSignpostType,
  dso: UnsafeRawPointer = #dsohandle,
  log: OSLog,
  name: StaticString,
  signpostID: OSSignpostID = .exclusive,
  _ format: StaticString,
  _ arguments: CVarArg...
) {
  let hasValidID = signpostID != .invalid && signpostID != .null
  guard log.signpostsEnabled && hasValidID else { return }
  let ra = _swift_os_log_return_address()
  name.withUTF8Buffer { (nameBuf: UnsafeBufferPointer<UInt8>) in
    // Since dladdr is in libc, it is safe to unsafeBitCast
    // the cstring argument type.
    nameBuf.baseAddress!.withMemoryRebound(
      to: CChar.self, capacity: nameBuf.count
    ) { nameStr in
      format.withUTF8Buffer { (formatBuf: UnsafeBufferPointer<UInt8>) in
        // Since dladdr is in libc, it is safe to unsafeBitCast
        // the cstring argument type.
        formatBuf.baseAddress!.withMemoryRebound(
          to: CChar.self, capacity: formatBuf.count
        ) { formatStr in
          withVaList(arguments) { valist in
            _swift_os_signpost_with_format(dso, ra, log, type,
                nameStr, signpostID.rawValue, formatStr, valist)
          }
        }
      }
    }
  }
}

@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
extension OSSignpostType {
  public static let event = __OS_SIGNPOST_EVENT
  public static let begin = __OS_SIGNPOST_INTERVAL_BEGIN
  public static let end = __OS_SIGNPOST_INTERVAL_END
}

@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public struct OSSignpostID {
  public let rawValue: os_signpost_id_t

  public static let exclusive = OSSignpostID(_swift_os_signpost_id_exclusive())
  public static let invalid = OSSignpostID(_swift_os_signpost_id_invalid())
  public static let null = OSSignpostID(_swift_os_signpost_id_null())

  public init(log: OSLog) {
    self.rawValue = __os_signpost_id_generate(log)
  }

  public init(log: OSLog, object: AnyObject) {
    self.rawValue = __os_signpost_id_make_with_pointer(log,
        UnsafeRawPointer(Unmanaged.passUnretained(object).toOpaque()))
  }

  public init(_ value: UInt64) {
    self.rawValue = value
  }
}

@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
extension OSSignpostID : Comparable {
  public static func < (a: OSSignpostID, b: OSSignpostID) -> Bool {
    return a.rawValue < b.rawValue
  }

  public static func == (a: OSSignpostID, b: OSSignpostID) -> Bool {
    return a.rawValue == b.rawValue
  }
}

@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
extension OSLog {
  public struct Category {
    public let rawValue: String
    public static let pointsOfInterest =
        Category(string: String(cString: _swift_os_signpost_points_of_interest()))
    private init(string: String) {
      self.rawValue = string
    }
  }

  public convenience init(subsystem: String, category: Category) {
    self.init(__subsystem: subsystem, category: category.rawValue)
  }

  public var signpostsEnabled: Bool {
    return __os_signpost_enabled(self)
  }
}
