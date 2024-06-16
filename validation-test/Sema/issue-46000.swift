// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=macosx

// https://github.com/apple/swift/issues/46000

import Dispatch
import Foundation

extension DispatchData {

  func asFoundationData<T>(execute: (Data) throws -> T) rethrows -> T {

    //FIXME: SWIFT(SR-3097) - DispatchData.withUnsafeBytes crashes when empty.
    guard isEmpty == false else {
      return try execute(Data())
    }

    return try withUnsafeBytes { (ptr: UnsafePointer<Int8>) -> Void in
      // expected-error@-1 {{cannot convert return expression of type 'Void' to return type 'T'}}
      let foundationData = Data(bytesNoCopy: UnsafeMutableRawPointer(mutating: ptr), count: count, deallocator: .none)
      return try execute(foundationData) // expected-error {{cannot convert value of type 'T' to closure result type 'Void'}}
    }
  }
}
