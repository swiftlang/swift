// RUN: %empty-directory(%t)

// 1. Make sure you can't -emit-ir or -c when you're skipping non-inlinable function bodies

// RUN: not %target-swift-frontend -emit-ir %s -experimental-skip-non-inlinable-function-bodies %s 2>&1 | %FileCheck %s --check-prefix ERROR
// RUN: not %target-swift-frontend -c %s -experimental-skip-non-inlinable-function-bodies %s 2>&1 | %FileCheck %s --check-prefix ERROR
// ERROR: -experimental-skip-non-inlinable-function-bodies does not support emitting IR

// 2. Emit the SIL for a module and check it against the expected strings in function bodies.
//    If we're doing the right skipping, then the strings that are CHECK-NOT'd won't have been typechecked or SILGen'd.

// RUN: %target-swift-frontend -emit-sil -O -experimental-skip-non-inlinable-function-bodies %s 2>&1 | %FileCheck %s --check-prefixes CHECK,CHECK-SIL-ONLY

// 3. Emit the module interface while skipping non-inlinable function bodies. Check it against the same set of strings.

// RUN: %target-swift-frontend -typecheck %s -enable-library-evolution -emit-module-interface-path %t/Module.skipping.swiftinterface -experimental-skip-non-inlinable-function-bodies
// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-INTERFACE-ONLY < %t/Module.skipping.swiftinterface

// 4. Emit the module interface normally. Also check it against the same set of strings.

// RUN: %target-swift-frontend -typecheck %s -enable-library-evolution -emit-module-interface-path %t/Module.swiftinterface
// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-INTERFACE-ONLY < %t/Module.swiftinterface

// 5. The module interfaces should be exactly the same.

// RUN: diff -u %t/Module.skipping.swiftinterface %t/Module.swiftinterface

@usableFromInline
@inline(never)
func _blackHole(_ s: String) {}

public struct Struct {
  @inlinable public func inlinableFunc() {
    _blackHole("@inlinable method body") // CHECK: @inlinable method body
  }

  @inline(__always)
  public func inlineAlwaysFunc() {
    _blackHole("@inline(__always) method body") // CHECK-NOT: @inline(__always) method body
  }

  @_transparent
  public func transparentFunc() {
    _blackHole("@_transparent method body") // CHECK: @_transparent method body
  }

  func internalFunc() {
    _blackHole("internal method body") // CHECK-NOT: internal method body
  }

  public func publicFunc() {
    _blackHole("public method body") // CHECK-NOT: public method body
  }

  private func privateFunc() {
    _blackHole("private method body") // CHECK-NOT: private method body
  }

  @inlinable public init() {
    _blackHole("@inlinable init body") // CHECK: @inlinable init body
  }

  @inline(__always) public init(a: Int) {
    _blackHole("@inline(__always) init body") // CHECK-NOT: @inline(__always) init body
  }

  @_transparent public init(b: Int) {
    _blackHole("@_transparent init body") // CHECK: @_transparent init body
  }

  init(c: Int) {
    _blackHole("internal init body") // CHECK-NOT: internal init body
  }

  public init(d: Int) {
    _blackHole("public init body") // CHECK-NOT: public init body
  }

  private init(e: Int) {
    _blackHole("private init body") // CHECK-NOT: private init body
  }

  @inlinable public subscript() -> Int {
    _blackHole("@inlinable subscript getter") // CHECK: @inlinable subscript getter
    return 0
  }

  @inline(__always) public subscript(a: Int, b: Int) -> Int {
    _blackHole("@inline(__always) subscript getter") // CHECK-NOT: @inline(__always) subscript getter
    return 0
  }

  public subscript(a: Int, b: Int, c: Int) -> Int {
    @_transparent get {
      _blackHole("@_transparent subscript getter") // CHECK: @_transparent subscript getter
      return 0
    }
  }

  subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
    _blackHole("internal subscript getter") // CHECK-NOT: internal subscript getter
    return 0
  }

  public subscript(a: Int, b: Int, c: Int, d: Int, e: Int) -> Int {
    _blackHole("public subscript getter") // CHECK-NOT: public subscript getter
    return 0
  }

  private subscript(e: Int) -> Int {
    _blackHole("private subscript getter") // CHECK-NOT: private subscript getter
    return 0
  }

  @inlinable public var inlinableVar: Int {
    _blackHole("@inlinable getter body") // CHECK: @inlinable getter body
    return 0
  }

  @inline(__always) public var inlineAlwaysVar: Int {
    _blackHole("@inline(__always) getter body") // CHECK-NOT: @inline(__always) getter body
    return 0
  }

  @_transparent public var transparentVar: Int {
    _blackHole("@_transparent getter body") // CHECK: @_transparent getter body
    return 0
  }

  public var publicVar: Int {
    _blackHole("public getter body") // CHECK-NOT: public getter body
    return 0
  }

  public var inlinableSetter: Int {
    get { 0 }
    @inlinable set {
      _blackHole("@inlinable setter body") // CHECK: @inlinable setter body
    }
  }

  public var inlineAlwaysSetter: Int {
    get { 0 }
    @inline(__always) set {
      _blackHole("@inline(__always) setter body") // CHECK-NOT: @inline(__always) setter body
    }
  }

  public var regularSetter: Int {
    get { 0 }
    set {
      _blackHole("@inline(__always) setter body") // CHECK-NOT: regular setter body
    }
  }
}

@_fixed_layout
public class InlinableDesubscript {
  @inlinable deinit {
    _blackHole("@inlinable deinit body") // CHECK: @inlinable deinit body
  }
}

@_fixed_layout
public class InlineAlwaysDeinit {
  @inline(__always) deinit {
    _blackHole("@inline(__always) deinit body") // CHECK-NOT: @inline(__always) deinit body
  }
}

public class NormalDeinit {
  deinit {
    _blackHole("regular deinit body") // CHECK-NOT: regular deinit body
  }
}

@inlinable public func inlinableFunc() {
  _blackHole("@inlinable func body") // CHECK: @inlinable func body
}

@inline(__always) public func inlineAlwaysFunc() {
  _blackHole("@inline(__always) func body") // CHECK-NOT: @inline(__always) func body
}

@_transparent public func transparentFunc() {
  _blackHole("@_transparent func body") // CHECK: @_transparent func body
}

func internalFunc() {
  _blackHole("internal func body") // CHECK-NOT: internal func body
}

public func publicFunc() {
  _blackHole("public func body") // CHECK-NOT: public func body
}

private func privateFunc() {
  _blackHole("private func body") // CHECK-NOT: private func body
}

@inlinable
public func inlinableLocalTypeFunc() {
  typealias InlinableLocalType = Int
  _blackHole("@inlinable func body with local type") // CHECK: @inlinable func body with local type
  func takesInlinableLocalType(_ x: InlinableLocalType) {
    _blackHole("nested func body inside @inlinable func body taking local type") // CHECK: nested func body inside @inlinable func body taking local type
  }
  takesInlinableLocalType(0)
}

@inline(__always) public func inlineAlwaysLocalTypeFunc() {
  typealias InlineAlwaysLocalType = Int
  _blackHole("@inline(__always) func body with local type") // CHECK-NOT: @inline(__always) func body with local type
  func takesInlineAlwaysLocalType(_ x: InlineAlwaysLocalType) {
    _blackHole("nested func body inside @inline(__always) func body taking local type") // CHECK-NOT: nested func body inside @inline(__always) func body taking local type
  }
  takesInlineAlwaysLocalType(0)
}

@_transparent public func _transparentLocalTypeFunc() {
  typealias TransparentLocalType = Int
  _blackHole("@_transparent func body with local type") // CHECK: @_transparent func body with local type
  func takesTransparentLocalType(_ x: TransparentLocalType) {
    _blackHole("nested func body inside @_transparent func body taking local type") // CHECK: nested func body inside @_transparent func body taking local type
  }
  takesTransparentLocalType(0)
}

public func publicLocalTypeFunc() {
  typealias LocalType = Int
  _blackHole("public func body with local type") // CHECK-NOT: public func body with local type
  func takesLocalType(_ x: LocalType) {
    _blackHole("nested func body inside public func body taking local type") // CHECK-NOT: nested func body inside public func body taking local type
  }
  takesLocalType(0)
}
@inlinable
public func inlinableNestedLocalTypeFunc() {
  func nestedFunc() {
    _blackHole("nested func body inside @inlinable func body") // CHECK: nested func body inside @inlinable func body
    typealias InlinableNestedLocalType = Int
    func takesLocalType(_ x: InlinableNestedLocalType) {
      _blackHole("nested func body inside @inlinable func body taking local type") // CHECK: nested func body inside @inlinable func body taking local type
    }
    takesLocalType(0)
  }
  nestedFunc()
}
