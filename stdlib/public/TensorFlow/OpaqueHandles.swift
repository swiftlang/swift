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

/// A class that implements common functionality for resource and variant handles.
public class ResourceVariantHandleBase {
	public let cTensorHandle: CTensorHandle
	public let cDataType: TF_DataType
	
  @usableFromInline
  init(owning cTensorHandle: CTensorHandle, cDataType : TF_DataType) {
    self.cTensorHandle = cTensorHandle
		self.cDataType = cDataType
  }

  deinit {
    debugLog("De-initializing TensorHandle.")
    TFE_DeleteTensorHandle(cTensorHandle)
    debugLog("Returning from deinit of handle.")
  }

	static public func getTFDataTypeName(_ type: TF_DataType) -> String {
		if type == TF_VARIANT {
			return "variant"
		}
		if type == TF_RESOURCE {
			return "resource"
		}
		return "unknown"
	}
	
	@inlinable
	static func receiveTensorHandleHelper(
		_ computation: _TensorComputation,
    _ tensorID: Int,
		_ dataType: TF_DataType) -> CTensorHandle {
		debugLog("Receiving \(ResourceVariantHandleBase.getTFDataTypeName(dataType)) tensor of id \(tensorID).")
    let status = TF_NewStatus()
    let context = _ExecutionContext.global
    let cTensorHandle: CTensorHandle! = TFE_DequeueNamedTensorFromCtx(
      context.eagerContext, Int32(tensorID), dataType, status)
    checkOk(status)
    TF_DeleteStatus(status)
		return cTensorHandle
	}

  @inlinable
  func sendToAcceleratorHelper(_ computation: _TensorComputation,
                         _ tensorID: Int) {
    debugLog("Sending \(ResourceVariantHandleBase.getTFDataTypeName(self.cDataType)) tensor of id \(tensorID).")
    let status = TF_NewStatus()
    let context = _ExecutionContext.global
    TFE_EnqueueNamedTensorFromCtx(
      context.eagerContext, Int32(tensorID), self.cTensorHandle, status)
    TF_DeleteStatus(status)
    debugLog("Done sending \(ResourceVariantHandleBase.getTFDataTypeName(self.cDataType)) tensor of id \(tensorID).")
  }
}


/// `ResourceHandle` is the type used by ops and the `#tfop()` syntax to
/// represent TensorFlow "resource" values.  
public final class ResourceHandle : ResourceVariantHandleBase {
  @usableFromInline
  init(owning cTensorHandle: CTensorHandle) {
		super.init(owning: cTensorHandle, cDataType: TF_RESOURCE)
  }
}


extension ResourceHandle : TensorSendableReceivable {
  @inlinable
  static func receiveFromAccelerator(
    _ computation: _TensorComputation,
    _ tensorID: Int
  ) -> ResourceHandle {
		let receivedTensor = receiveTensorHandleHelper(computation, tensorID, TF_RESOURCE)
		return ResourceHandle(owning: receivedTensor)
	}

  @inlinable
  func sendToAccelerator(_ computation: _TensorComputation,
    _ tensorID: Int) {
		self.sendToAcceleratorHelper(computation, tensorID);
  }

  // TODO: remove this dummy Scalar typealias, currently required in order to
  // conform to TensorSendableReceivable.
  typealias Scalar = Float
  @inlinable
  static func scalar(_ scalar: Scalar) -> ResourceHandle {
    fatalError("Unsupported")
  }
}


/// `VariantHandle` is the type used by ops and the `#tfop()` syntax to
/// represent TensorFlow "variant" values.
/// TODO: rename this source file as this is no longer "opaque".
public final class VariantHandle : ResourceVariantHandleBase {
  @usableFromInline
  init(owning cTensorHandle: CTensorHandle) {
		super.init(owning: cTensorHandle, cDataType: TF_VARIANT)
  }
}

extension VariantHandle : TensorSendableReceivable {
  @inlinable
  static func receiveFromAccelerator(
    _ computation: _TensorComputation,
    _ tensorID: Int
  ) -> VariantHandle {
		let receivedTensor = receiveTensorHandleHelper(computation, tensorID, TF_VARIANT)
		return VariantHandle(owning: receivedTensor)
	}

  @inlinable
  func sendToAccelerator(_ computation: _TensorComputation,
    _ tensorID: Int) {
		self.sendToAcceleratorHelper(computation, tensorID);
  }

  // TODO: remove this dummy Scalar typealias, currently required in order to
  // conform to TensorSendableReceivable.
  typealias Scalar = Float
  @inlinable
  static func scalar(_ scalar: Scalar) -> VariantHandle {
    fatalError("Unsupported")
  }
}
