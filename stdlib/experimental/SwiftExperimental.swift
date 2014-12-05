//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// Experimental APIs of the Swift Standard Library
//
// This library contains experimental APIs that can be subject to change or
// removal.  We don't guarantee API or ABI stability for this library.
//
//===----------------------------------------------------------------------===//

/// The function composition operator is the only user-defined operator that
/// operates on functions.  That's why the numeric value of precedence does
/// not matter right now.
infix operator ○ { associativity left precedence 100 }

/// Compose functions.
///
/// ::
///
///   (g ○ f)(x) == g(f(x))
///
/// :returns: a function that applies ``g`` to the result of applying ``f``
/// to the argument of the new function.
public func ○<T, U, V>(g: U -> V, f: T -> U) -> (T -> V) {
  return { g(f($0)) }
}

