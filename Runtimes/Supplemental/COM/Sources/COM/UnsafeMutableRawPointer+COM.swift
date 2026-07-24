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

extension UnsafeMutableRawPointer {
  /// Creates a raw pointer from the COM interface pointer of a `@com` object.
  ///
  /// The returned pointer points to the vtable pointer in the object's COM block.
  /// It is borrowed and valid for the lifetime of the object. No `AddRef` is performed.
  ///
  /// - Parameter unknown: The COM object to extract the interface pointer from.
  @_transparent
  public init<Interface>(unsafeCOMPointer pUnk: borrowing Interface) where Interface.Type: COMInterface {
    // A `@com` object's Swift reference is its heap-object pointer `P`; the
    // primary COM interface pointer is the vtable-pointer slot at `P[-2]`.
    // `passUnretained(_:).toOpaque()` is the inverse of the `fromOpaque(_:)`
    // recovery used elsewhere in the module and performs no retain.
    let P = Unmanaged.passUnretained(pUnk).toOpaque()
    self = P.advanced(by: -2 * MemoryLayout<UnsafeRawPointer>.stride)
  }
}
