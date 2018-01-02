//===-- TensorHandle.swift ------------------------------------*- swift -*-===//
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
// This file defines the TensorHandle and AnyTensorHandle types.
//
//===----------------------------------------------------------------------===//

// This corresponds to the TensorHandle class in TF eager C API.
public class AnyTensorHandle {
  // TODO(hongm): replace this dummy impl with something like
  // UnsafePointer<TF_TensorHandle>
  // FIXME: Implement in terms of a TensorFlow TensorHandle, using the C API.
}

/// TensorHandle<T> is the type used by "ops" and the #tfop() syntax
/// specifically.  It includes an element type, which the tf-compiler internals
/// depend on to know what the dtype of params are when they are extracted out
/// into a tensor program.
public class TensorHandle<T: TensorElementProtocol> : AnyTensorHandle {
}

// For "print", REPL, and Playgrounds integeration, we'll eventually want to
// implement this, probably in terms of fetching a summary.  For now, this is
// disabled.
#if false
/// Make "print(someTensor)" print a pretty form of the tensor.
extension TensorHandle : CustomStringConvertible {
  public var description: String {
    fatalError("unimplemented")
  }
}

// Make Tensors show up nicely in the Xcode Playground results sidebar.
extension TensorHandle : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(description)
  }
}
#endif





