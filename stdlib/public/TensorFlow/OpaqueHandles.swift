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
/// represent TensorFlow "variant" values.  It exists only to represent edges
/// in TensorFlow graphs, and has no host-side representation (and thus no
/// methods).
public final class VariantHandle {
  private init() {
    fatalError("VariantHandle is a marker type that can never be created")
  }
}
