//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import CxxShim

/// Deconstructs a Swift closure into a pair of a function pointer and a context
/// pointer. The context is retained.
@_semantics("optimize.sil.specialize.generic.never")
public func __convertToRawClosure<A>(_ closure: @escaping () -> A) -> __swift_interop_closure {
  return unsafe withExtendedLifetime(closure) {
    let r = unsafe unsafeBitCast(closure, to: __swift_interop_closure.self)
    unsafe r.retain()
    return unsafe r
  }
}

@_semantics("optimize.sil.specialize.generic.never")
public func __convertToRawClosure<A, B>(_ closure: @escaping (A) -> B) -> __swift_interop_closure {
  return unsafe withExtendedLifetime(closure) {
    let r = unsafe unsafeBitCast(closure, to: __swift_interop_closure.self)
    unsafe r.retain()
    return unsafe r
  }
}

@_semantics("optimize.sil.specialize.generic.never")
public func __convertToRawClosure<A, B, C>(_ closure: @escaping (A, B) -> C) -> __swift_interop_closure {
  return unsafe withExtendedLifetime(closure) {
    let r = unsafe unsafeBitCast(closure, to: __swift_interop_closure.self)
    unsafe r.retain()
    return unsafe r
  }
}

@_semantics("optimize.sil.specialize.generic.never")
public func __convertToRawClosure<A, B, C, D>(_ closure: @escaping (A, B, C) -> D) -> __swift_interop_closure {
  return unsafe withExtendedLifetime(closure) {
    let r = unsafe unsafeBitCast(closure, to: __swift_interop_closure.self)
    unsafe r.retain()
    return unsafe r
  }
}

@_semantics("optimize.sil.specialize.generic.never")
public func __convertToRawClosure<A, B, C, D, E>(_ closure: @escaping (A, B, C, D) -> E) -> __swift_interop_closure {
  return unsafe withExtendedLifetime(closure) {
    let r = unsafe unsafeBitCast(closure, to: __swift_interop_closure.self)
    unsafe r.retain()
    return unsafe r
  }
}

@_semantics("optimize.sil.specialize.generic.never")
public func __convertToRawClosure<A, B, C, D, E, F>(_ closure: @escaping (A, B, C, D, E) -> F) -> __swift_interop_closure {
  return unsafe withExtendedLifetime(closure) {
    let r = unsafe unsafeBitCast(closure, to: __swift_interop_closure.self)
    unsafe r.retain()
    return unsafe r
  }
}
