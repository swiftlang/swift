// RUN: %empty-directory(%t)

// REQUIRES: rdar95219987

// Check -emit-ir and -c are invalid when skipping function bodies
// RUN: not %target-swift-frontend -emit-ir %s -experimental-skip-non-inlinable-function-bodies %s 2>&1 | %FileCheck %s --check-prefix ERROR
// RUN: not %target-swift-frontend -c %s -experimental-skip-non-inlinable-function-bodies %s 2>&1 | %FileCheck %s --check-prefix ERROR
// RUN: not %target-swift-frontend -emit-ir %s -experimental-skip-non-inlinable-function-bodies-without-types %s 2>&1 | %FileCheck %s --check-prefix ERROR
// RUN: not %target-swift-frontend -c %s -experimental-skip-non-inlinable-function-bodies-without-types %s 2>&1 | %FileCheck %s --check-prefix ERROR
// RUN: not %target-swift-frontend -emit-ir %s -experimental-skip-all-function-bodies %s 2>&1 | %FileCheck %s --check-prefix ERROR
// RUN: not %target-swift-frontend -c %s -experimental-skip-all-function-bodies %s 2>&1 | %FileCheck %s --check-prefix ERROR
// ERROR: the -experimental-skip-*-function-bodies* flags do not support emitting IR

// Warn when trying to build SwiftONoneSupport with any skip enabled
// RUN: %target-swift-frontend -typecheck -experimental-skip-non-inlinable-function-bodies -module-name SwiftOnoneSupport %s 2>&1 | %FileCheck %s --check-prefix WARNING
// RUN: %target-swift-frontend -typecheck -experimental-skip-non-inlinable-function-bodies-without-types -module-name SwiftOnoneSupport %s 2>&1 | %FileCheck %s --check-prefix WARNING
// RUN: %target-swift-frontend -typecheck -experimental-skip-all-function-bodies -module-name SwiftOnoneSupport %s 2>&1 | %FileCheck %s --check-prefix WARNING
// WARNING: module 'SwiftOnoneSupport' cannot be built with any of the -experimental-skip-*-function-bodies* flags; they have been automatically disabled

// Check skipped bodies are neither typechecked nor SILgen'd
// RUN: %target-swift-frontend -emit-sil -emit-sorted-sil -experimental-skip-non-inlinable-function-bodies -debug-forbid-typecheck-prefix NEVERTYPECHECK -debug-forbid-typecheck-prefix INLINENOTYPECHECK %s -o %t/Skip.noninlinable.sil
// RUN: %target-swift-frontend -emit-sil -emit-sorted-sil -experimental-skip-non-inlinable-function-bodies-without-types -debug-forbid-typecheck-prefix NEVERTYPECHECK -debug-forbid-typecheck-prefix TYPESNOTYPECHECK %s -o %t/Skip.withouttypes.sil
// RUN: %target-swift-frontend -emit-sil -emit-sorted-sil -experimental-skip-all-function-bodies -debug-forbid-typecheck-prefix NEVERTYPECHECK -debug-forbid-typecheck-prefix ALLNOTYPECHECK %s -o %t/Skip.all.sil
// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-NONINLINE-ONLY,CHECK-NONINLINE-SIL < %t/Skip.noninlinable.sil
// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-WITHOUTTYPES-ONLY,CHECK-NONINLINE-SIL < %t/Skip.withouttypes.sil
// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-ALL-ONLY < %t/Skip.all.sil

// Emit the module interface and check it against the same set of strings.
// RUN: %target-swift-frontend -typecheck %s -enable-library-evolution -emit-module-interface-path %t/Skip.noninlinable.swiftinterface -experimental-skip-non-inlinable-function-bodies
// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-NONINLINE-ONLY,CHECK-NONINLINE-TEXTUAL < %t/Skip.noninlinable.swiftinterface
// RUN: %target-swift-frontend -typecheck %s -enable-library-evolution -emit-module-interface-path %t/Skip.all.swiftinterface -experimental-skip-all-function-bodies
// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-ALL-ONLY,CHECK-NONINLINE-TEXTUAL < %t/Skip.all.swiftinterface

// Emit the module interface normally, it should be the same as when skipping
// non-inlinable.
// RUN: %target-swift-frontend -typecheck %s -enable-library-evolution -emit-module-interface-path %t/Skip.swiftinterface
// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-NONINLINE-ONLY,CHECK-NONINLINE-TEXTUAL < %t/Skip.swiftinterface
// RUN: diff -u %t/Skip.noninlinable.swiftinterface %t/Skip.swiftinterface

@usableFromInline
@inline(never)
func _blackHole(_ s: String) {}

// NOTE: The order of the checks below is important. The checks try to be
// logically grouped, but sometimes -emit-sorted-sil needs to break the logical
// order.

@inlinable public func inlinableFunc() {
  let ALLNOTYPECHECK_local = 1
  _blackHole("@inlinable func body")
  // CHECK-NONINLINE-ONLY "@inlinable func body"
  // CHECK-ALL-ONLY-NOT: "@inlinable func body"
}

@_fixed_layout
public class InlinableDeinit {
  @inlinable deinit {
    let ALLNOTYPECHECK_local = 1
    _blackHole("@inlinable deinit body")
    // CHECK-NONINLINE-ONLY: "@inlinable deinit body"
    // CHECK-ALL-ONLY-NOT: "@inlinable deinit body"
  }
}

@_fixed_layout
public class InlineAlwaysDeinit {
  @inline(__always) deinit {
    let NEVERTYPECHECK_local = 1
    _blackHole("@inline(__always) deinit body") // CHECK-NOT: "@inline(__always) deinit body"
  }
}

public class NormalDeinit {
  deinit {
    let NEVERTYPECHECK_local = 1
    _blackHole("regular deinit body") // CHECK-NOT: "regular deinit body"
  }
}

@_transparent public func transparentFunc() {
  let ALLNOTYPECHECK_local = 1
  _blackHole("@_transparent func body")
  // CHECK-NONINLINE-ONLY: "@_transparent func body"
  // CHECK-ALL-ONLY-NOT: "@_transparent func body"
}

@inline(__always) public func inlineAlwaysFunc() {
  let NEVERTYPECHECK_local = 1
  _blackHole("@inline(__always) func body") // CHECK-NOT: "@inline(__always) func body"
}

func internalFunc() {
  let NEVERTYPECHECK_local = 1
  _blackHole("internal func body") // CHECK-NOT: "internal func body"
}

public func publicFunc() {
  let NEVERTYPECHECK_local = 1
  _blackHole("public func body") // CHECK-NOT: "public func body"
}

private func privateFunc() {
  let NEVERTYPECHECK_local = 1
  _blackHole("private func body") // CHECK-NOT: "private func body"
}

@inline(__always) public func inlineAlwaysLocalTypeFunc() {
  let NEVERTYPECHECK_outerLocal = 1

  typealias InlineAlwaysLocalType = Int
  _blackHole("@inline(__always) func body with local type") // CHECK-NOT: "@inline(__always) func body with local type"
  func takesInlineAlwaysLocalType(_ x: InlineAlwaysLocalType) {
    let NEVERTYPECHECK_innerLocal = 1
    _blackHole("nested func body inside @inline(__always) func body taking local type") // CHECK-NOT: "nested func body inside @inline(__always) func body taking local type"
  }
  takesInlineAlwaysLocalType(0)
}

public func publicLocalTypeFunc() {
  let NEVERTYPECHECK_outerLocal = 1

  typealias LocalType = Int
  _blackHole("public func body with local type") // CHECK-NOT: "public func body with local type"
  func takesLocalType(_ x: LocalType) {
    let NEVERTYPECHECK_innerLocal = 1
    _blackHole("nested func body inside public func body taking local type") // CHECK-NOT: "nested func body inside public func body taking local type"
  }
  takesLocalType(0)
}

@inlinable
public func inlinableLocalTypeFunc() {
  let ALLNOTYPECHECK_outerLocal = 1

  typealias InlinableLocalType = Int
  _blackHole("@inlinable func body with local type")
  // CHECK-NONINLINE-ONLY: "@inlinable func body with local type"
  // CHECK-ALL-ONLY-NOT: "@inlinable func body with local type"

  func takesInlinableLocalType(_ x: InlinableLocalType) {
    let ALLNOTYPECHECK_innerLocal = 1
    _blackHole("nested func body inside @inlinable func body taking local type")
    // CHECK-NONINLINE-ONLY: "nested func body inside @inlinable func body taking local type"
    // CHECK-ALL-ONLY-NOT: "nested func body inside @inlinable func body taking local type"
  }
  takesInlinableLocalType(0)
}

@_transparent public func _transparentLocalTypeFunc() {
  let ALLNOTYPECHECK_outerLocal = 1

  typealias TransparentLocalType = Int
  _blackHole("@_transparent func body with local type")
  // CHECK-NONINLINE-ONLY: "@_transparent func body with local type"
  // CHECK-ALL-ONLY-NOT: "@_transparent func body with local type"

  func takesTransparentLocalType(_ x: TransparentLocalType) {
    let ALLNOTYPECHECK_innerLocal = 1
    _blackHole("nested func body inside @_transparent func body taking local type")
    // CHECK-NONINLINE-ONLY: "nested func body inside @_transparent func body taking local type"
    // CHECK-ALL-ONLY-NOT: "nested func body inside @_transparent func body taking local type"
  }
  takesTransparentLocalType(0)
}

@inlinable
public func inlinableNestedLocalTypeFunc() {
  let ALLNOTYPECHECK_outerLocal = 1

  func nestedFunc() {
    let ALLNOTYPECHECK_innerLocal = 1
    _blackHole("nested func body inside @inlinable func body")
    // CHECK-NONINLINE-ONLY: "nested func body inside @inlinable func body"
    // CHECK-ALL-ONLY-NOT: "nested func body inside @inlinable func body"

    typealias InlinableNestedLocalType = Int
    func takesLocalType(_ x: InlinableNestedLocalType) {
      let ALLNOTYPECHECK_innerLocal2 = 1
      _blackHole("nested func body inside @inlinable func body taking local type")
      // CHECK-NONINLINE-ONLY: "nested func body inside @inlinable func body taking local type"
      // CHECK-ALL-ONLY-NOT: "nested func body inside @inlinable func body taking local type"
    }
    takesLocalType(0)
  }
  nestedFunc()
}

public func funcWithEnum() {
  let INLINENOTYPECHECK_local = 1
  let ALLNOTYPECHECK_local = 1
  _blackHole("func with enum body")
  // CHECK-WITHOUTTYPES-ONLY: "func with enum body"
  // CHECK-NONINLINE-ONLY-NOT: "func with enum body"
  // CHECK-ALL-ONLY-NOT: "func with enum body"
  enum E {}
}

public func funcWithClass() {
  let INLINENOTYPECHECK_local = 1
  let ALLNOTYPECHECK_local = 1
  _blackHole("func with class body")
  // CHECK-WITHOUTTYPES-ONLY: "func with class body"
  // CHECK-NONINLINE-ONLY-NOT: "func with class body"
  // CHECK-ALL-ONLY-NOT: "func with class body"
  class C {}
}

public func funcWithStruct() {
  let INLINENOTYPECHECK_local = 1
  let ALLNOTYPECHECK_local = 1
  _blackHole("func with struct body")
  // CHECK-WITHOUTTYPES-ONLY: "func with struct body"
  // CHECK-NONINLINE-ONLY-NOT: "func with struct body"
  // CHECK-ALL-ONLY-NOT: "func with struct body"
  struct S {}
}

public func funcWithNestedFuncs() {
  let INLINENOTYPECHECK_local = 1
  let ALLNOTYPECHECK_local = 1
  _blackHole("func with nested funcs body")
  // CHECK-WITHOUTTYPES-ONLY: "func with nested funcs body"
  // CHECK-NONINLINE-ONLY-NOT: "func with nested funcs body"
  // CHECK-ALL-ONLY-NOT: "func with nested funcs body"

  func bar() {
    _blackHole("nested func body")
    // CHECK-WITHOUTTYPES-ONLY: "nested func body"
    // FIXME: We could skip this nested function.
  }

  func foo() {
    _blackHole("nested func with type body")
    // CHECK-WITHOUTTYPES-ONLY: "nested func with type body"
    struct S {}
  }
}

public struct Struct {
  @inlinable public var inlinableVar: Int {
    let ALLNOTYPECHECK_local = 1
    _blackHole("@inlinable getter body")
    // CHECK-NONINLINE-ONLY: "@inlinable getter body"
    // CHECK-ALL-ONLY-NOT: "@inlinable getter body"
    return 0
  }

  @inlinable public func inlinableFunc() {
    let ALLNOTYPECHECK_local = 1
    _blackHole("@inlinable method body")
    // CHECK-NONINLINE-ONLY: "@inlinable method body"
    // CHECK-ALL-ONLY-NOT: "@inlinable method body"
  }

  @inline(__always)
  public func inlineAlwaysFunc() {
    let NEVERTYPECHECK_local = 1
    _blackHole("@inline(__always) method body") // CHECK-NOT: "@inline(__always) method body"
  }

  @_transparent public var transparentVar: Int {
    let ALLNOTYPECHECK_local = 1
    _blackHole("@_transparent getter body")
    // CHECK-NONINLINE-ONLY: "@_transparent getter body"
    // CHECK-ALL-ONLY-NOT: "@_transparent getter body"
    return 0
  }

  public var inlinableSetter: Int {
    get { 0 }
    @inlinable set {
      let ALLNOTYPECHECK_local = 1
      _blackHole("@inlinable setter body")
      // CHECK-NONINLINE-ONLY: "@inlinable setter body"
      // CHECK-ALL-ONLY-NOT: "@inlinable setter body"
    }
  }

  public var willSetVar: Int = 1 {
    willSet {
      let ALLNOTYPECHECK_local = 1
      _blackHole("willSet body") // CHECK-NOT: "willSet body"
    }
  }

  @_transparent
  public func transparentFunc() {
    let ALLNOTYPECHECK_local = 1
    _blackHole("@_transparent method body")
    // CHECK-NONINLINE-ONLY: "@_transparent method body"
    // CHECK-ALL-ONLY-NOT: "@_transparent method body"
  }

  func internalFunc() {
    let NEVERTYPECHECK_local = 1
    _blackHole("internal method body") // CHECK-NOT: "internal method body"
  }

  public func publicFunc() {
    let NEVERTYPECHECK_local = 1
    _blackHole("public method body") // CHECK-NOT: "public method body"
  }

  private func privateFunc() {
    let NEVERTYPECHECK_local = 1
    _blackHole("private method body") // CHECK-NOT: "private method body"
  }

  @_transparent public init(b: Int) {
    let ALLNOTYPECHECK_local = 1
    _blackHole("@_transparent init body")
    // CHECK-NONINLINE-ONLY: "@_transparent init body"
    // CHECK-ALL-ONLY-NOT: "@_transparent init body"
  }

  public var didSetVar: Int = 1 {
    didSet {
      // Body typechecked regardless
      _blackHole("didSet body") // CHECK-NONINLINE-SIL: "didSet body"
      // CHECK-NONINLINE-TEXTUAL-NOT: "didSet body"
    }
  }

  @inlinable public init() {
    let ALLNOTYPECHECK_local = 1
    _blackHole("@inlinable init body")
    // CHECK-NONINLINE-ONLY: "@inlinable init body"
    // CHECK-ALL-ONLY-NOT: "@inlinable init body"
  }

  @inline(__always) public init(a: Int) {
    let NEVERTYPECHECK_local = 1
    _blackHole("@inline(__always) init body") // CHECK-NOT: "@inline(__always) init body"
  }

  init(c: Int) {
    let NEVERTYPECHECK_local = 1
    _blackHole("internal init body") // CHECK-NOT: "internal init body"
  }

  public init(d: Int) {
    let NEVERTYPECHECK_local = 1
    _blackHole("public init body") // CHECK-NOT: "public init body"
  }

  private init(e: Int) {
    let NEVERTYPECHECK_local = 1
    _blackHole("private init body") // CHECK-NOT: "private init body"
  }

  @inlinable public subscript() -> Int {
    let ALLNOTYPECHECK_local = 1
    _blackHole("@inlinable subscript getter")
    // CHECK-NONINLINE-ONLY: "@inlinable subscript getter"
    // CHECK-ALL-ONLY-NOT: "@inlinable subscript getter"
    return 0
  }

  @inline(__always) public subscript(a: Int, b: Int) -> Int {
    let NEVERTYPECHECK_local = 1
    _blackHole("@inline(__always) subscript getter") // CHECK-NOT: "@inline(__always) subscript getter"
    return 0
  }

  public subscript(a: Int, b: Int, c: Int) -> Int {
    @_transparent get {
      let ALLNOTYPECHECK_local = 1
      _blackHole("@_transparent subscript getter")
      // CHECK-NONINLINE-ONLY: "@_transparent subscript getter"
      // CHECK-ALL-ONLY-NOT: "@_transparent subscript getter"
      return 0
    }
  }

  subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
    let NEVERTYPECHECK_local = 1
    _blackHole("internal subscript getter") // CHECK-NOT: "internal subscript getter"
    return 0
  }

  public subscript(a: Int, b: Int, c: Int, d: Int, e: Int) -> Int {
    let NEVERTYPECHECK_local = 1
    _blackHole("public subscript getter") // CHECK-NOT: "public subscript getter"
    return 0
  }

  private subscript(e: Int) -> Int {
    let NEVERTYPECHECK_local = 1
    _blackHole("private subscript getter") // CHECK-NOT: "private subscript getter"
    return 0
  }

  @inline(__always) public var inlineAlwaysVar: Int {
    let NEVERTYPECHECK_local = 1
    _blackHole("@inline(__always) getter body") // CHECK-NOT: "@inline(__always) getter body"
    return 0
  }

  public var publicVar: Int {
    let NEVERTYPECHECK_local = 1
    _blackHole("public getter body") // CHECK-NOT: "public getter body"
    return 0
  }

  public var inlineAlwaysSetter: Int {
    get { 0 }
    @inline(__always) set {
      let NEVERTYPECHECK_local = 1
      _blackHole("@inline(__always) setter body") // CHECK-NOT: "@inline(__always) setter body"
    }
  }

  public var regularSetter: Int {
    get { 0 }
    set {
      let NEVERTYPECHECK_local = 1
      _blackHole("@inline(__always) setter body") // CHECK-NOT: "regular setter body"
    }
  }
}

// Skipping all function bodies should skip all SIL
// CHECK-ALL-ONLY-NOT: sil_global
// CHECK-ALL-ONLY-NOT: sil_vtable
// CHECK-ALL-ONLY-NOT: sil_property
