//===-- OpaqueHandles.swift -----------------------------------*- swift -*-===//
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
//
// This file defines the ResourceHandle and VariantHandle types.
//
//===----------------------------------------------------------------------===//

import CTensorFlow

/// `ResourceHandle` is the type used by ops and the `#tfop()` syntax to
/// represent TensorFlow "resource" values.  It exists only to represent edges
/// in TensorFlow graphs, and has no host-side representation (and thus no
/// methods).
public final class ResourceHandle {
  private init() {
    fatalError("ResourceHandle is a marker type that can never be created")
  }
}

/// `VariantHandle` is the type used by ops and the `#tfop()` syntax to
/// represent TensorFlow "variant" values.
/// TODO: rename this source file as this is no longer "opaque".
public final class VariantHandle {
  public let cTensorHandle: CTensorHandle

  @usableFromInline
  init(owning cTensorHandle: CTensorHandle) {
    self.cTensorHandle = cTensorHandle
  }

  deinit {
    debugLog("De-initializing TensorHandle.")
    TFE_DeleteTensorHandle(cTensorHandle)
    debugLog("Returning from deinit of VariantHandle.")
  }
}

extension VariantHandle : TensorSendableReceivable {
  @inlinable
  static func receiveFromAccelerator(_ computation: _TensorComputation,
                                     _ tensorID: Int
  ) -> VariantHandle {
    debugLog("Receiving variant tensor of id \(tensorID).")
    let status = TF_NewStatus()
    let context = _ExecutionContext.global
    let cTensorHandle: CTensorHandle! = TFE_DequeueNamedTensorFromCtx(
      context.eagerContext, Int32(tensorID), TF_VARIANT, status)
    checkOk(status)
    TF_DeleteStatus(status)
    debugLog("Done receiving variant tensor of id \(tensorID).")
    return VariantHandle(owning: cTensorHandle)    
  }

  @inlinable
  func sendToAccelerator(_ computation: _TensorComputation,
                         _ tensorID: Int) {
    debugLog("Sending variant tensor of id \(tensorID).")
    let status = TF_NewStatus()
    let context = _ExecutionContext.global
    TFE_EnqueueNamedTensorFromCtx(
      context.eagerContext, Int32(tensorID), self.cTensorHandle, status)
    TF_DeleteStatus(status)
    debugLog("Done sending variant tensor of id \(tensorID).")
  }

  // TODO: remove this dummy Scalar typealias, currently required in order to
  // conform to TensorSendableReceivable.
  typealias Scalar = Float
  @inlinable
  static func scalar(_ scalar: Scalar) -> VariantHandle {
    fatalError("Unsupported");
  }
}
