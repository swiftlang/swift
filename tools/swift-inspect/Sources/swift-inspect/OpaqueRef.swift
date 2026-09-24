//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A strong reference to an object, presented as an opaque pointer for passing
/// through a C API's context parameter while keeping the object alive for the
/// duration. Suitable for temporary references in locals, or in stored
/// properties to keep an object alive for a longer time.
struct OpaqueRef: ~Copyable {
  private let object: AnyObject

  init(_ object: AnyObject) {
    self.object = object
  }

  /// The opaque pointer to pass to C.
  var pointer: UnsafeMutableRawPointer {
    Unmanaged.passUnretained(self.object).toOpaque()
  }
}
