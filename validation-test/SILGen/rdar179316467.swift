// RUN: %target-swift-emit-silgen -sil-verify-all -enable-library-evolution -enable-experimental-feature Lifetimes %s
// RUN: %target-swift-emit-sil -sil-verify-all -enable-library-evolution -enable-experimental-feature Lifetimes -verify %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: objc_interop

import Foundation

public struct SK {
  var obj: AnyObject = NSObject()
  public init<D: DataProtocol>(data: D) {}

  public var bytes: RawSpan {
    @_lifetime(borrow self)
    _read {
      yield _overrideLifetime(RawSpan(), borrowing: self)
    }
  }
}

public func consume(data: RawSpan, using key: SK) {}

public func extract(input: SK, salt: Data) {
  let key: SK
  if salt.regions.count != 1 {
    let bytes = Array(salt)
    key = SK(data: bytes)
  } else {
    key = SK(data: salt.regions.first!)
  }
  consume(data: input.bytes, using: key)
}
