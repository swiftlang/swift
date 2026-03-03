// RUN: %empty-directory(%t)

// Check skipped function bodies are neither typechecked nor SILgen'd when the
// -experimental-skip-*-function-bodies-* flags are specified.
// RUN: %target-swift-frontend -emit-sil -target %target-swift-5.10-abi-triple %s > %t/NoSkip.sil
// RUN: %target-swift-frontend -emit-sil -experimental-skip-non-inlinable-function-bodies -debug-forbid-typecheck-prefix NEVERTYPECHECK -debug-forbid-typecheck-prefix INLINENOTYPECHECK %s > %t/Skip.noninlinable.sil
// RUN: %target-swift-frontend -emit-sil -target %target-swift-5.10-abi-triple -experimental-skip-non-inlinable-function-bodies-without-types -debug-forbid-typecheck-prefix NEVERTYPECHECK %s > %t/Skip.withouttypes.sil

// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-SIL-NO-SKIP --input-file %t/NoSkip.sil
// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES,CHECK-SIL-SKIP-NONINLINE --input-file %t/Skip.noninlinable.sil
// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES,CHECK-SIL-SKIP-WITHOUTTYPES --input-file %t/Skip.withouttypes.sil

// RUN: %target-swift-frontend -emit-sil -experimental-skip-all-function-bodies -debug-forbid-typecheck-prefix NEVERTYPECHECK -debug-forbid-typecheck-prefix INLINENOTYPECHECK -debug-forbid-typecheck-prefix ALLNOTYPECHECK %s > %t/Skip.all.sil
// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-SIL-SKIP-ALL --input-file %t/Skip.all.sil

// Emit module interfaces and check their contents, too.
// RUN: %target-swift-emit-module-interface(%t/Skip.noninlinable.swiftinterface) %s -target %target-swift-5.10-abi-triple -module-name Skip -experimental-skip-non-inlinable-function-bodies
// RUN: %target-swift-typecheck-module-from-interface(%t/Skip.noninlinable.swiftinterface) -module-name Skip
// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-TEXTUAL --input-file %t/Skip.noninlinable.swiftinterface
// RUN: %target-swift-emit-module-interface(%t/Skip.all.swiftinterface) %s -target %target-swift-5.10-abi-triple -module-name Skip -experimental-skip-all-function-bodies
// RUN: %target-swift-typecheck-module-from-interface(%t/Skip.all.swiftinterface) -module-name Skip
// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-TEXTUAL --input-file %t/Skip.all.swiftinterface

// Verify that the emitted interfaces match an interface emitted without any
// body skipping flags.
// RUN: %target-swift-emit-module-interface(%t/NoSkip.swiftinterface) %s -target %target-swift-5.10-abi-triple -module-name Skip
// RUN: %FileCheck %s --check-prefixes CHECK,CHECK-TEXTUAL --input-file %t/NoSkip.swiftinterface
// RUN: diff -u %t/Skip.noninlinable.swiftinterface %t/NoSkip.swiftinterface
// RUN: diff -u %t/Skip.all.swiftinterface %t/NoSkip.swiftinterface

// Skipping all function bodies should skip *all* SIL.
// CHECK-SIL-SKIP-ALL: sil_stage canonical
// CHECK-SIL-SKIP-ALL-NOT: string_literal utf8
// CHECK-SIL-SKIP-ALL-NOT: sil_global
// CHECK-SIL-SKIP-ALL-NOT: sil_vtable
// CHECK-SIL-SKIP-ALL-NOT: sil_property

@usableFromInline
@inline(never)
func _blackHole(_ s: String) {}

@_fixed_layout
public class DeinitAlwaysInline {
  @inline(__always) deinit {
    let NEVERTYPECHECK_local = "DeinitAlwaysInline.deinit()"
    _blackHole(NEVERTYPECHECK_local)
  }
}
// CHECK-TEXTUAL-NOT: "DeinitAlwaysInline.deinit()"
// CHECK-SIL-NO-SKIP: "DeinitAlwaysInline.deinit()"
// CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "DeinitAlwaysInline.deinit()"

@_fixed_layout
public class DeinitInlinable {
  @inlinable deinit {
    let ALLNOTYPECHECK_local = "DeinitInlinable.deinit()"
    _blackHole(ALLNOTYPECHECK_local)
  }
}
// CHECK-TEXTUAL: "DeinitInlinable.deinit()"
// CHECK-SIL-NO-SKIP: "DeinitInlinable.deinit()"
// CHECK-SIL-SKIP-NONINLINE: "DeinitInlinable.deinit()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "DeinitInlinable.deinit()"

public class DeinitNormal {
  deinit {
    let NEVERTYPECHECK_local = "DeinitNormal.deinit()"
    _blackHole(NEVERTYPECHECK_local)
  }
}
// CHECK-TEXTUAL-NOT: "DeinitNormal.deinit()"
// CHECK-SIL-NO-SKIP: "DeinitNormal.deinit()"
// CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "DeinitNormal.deinit()"

@inline(__always) public func funcAlwaysInline() {
  let NEVERTYPECHECK_local = "funcAlwaysInline()"
  _blackHole(NEVERTYPECHECK_local)
}
// CHECK-TEXTUAL-NOT: "funcAlwaysInline()"
// CHECK-SIL-NO-SKIP: "funcAlwaysInline()"
// CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "funcAlwaysInline()"

@inlinable public func funcInlinable() {
  let ALLNOTYPECHECK_local = "funcInlinable()"
  _blackHole(ALLNOTYPECHECK_local)
}
// CHECK-TEXTUAL: "funcInlinable()"
// CHECK-SIL-NO-SKIP: "funcInlinable()"
// CHECK-SIL-SKIP-NONINLINE: "funcInlinable()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcInlinable()"

@inlinable public func funcInlinableWithDefer() {
  defer {
    let ALLNOTYPECHECK_local = "funcInlinableWithDefer()"
    _blackHole(ALLNOTYPECHECK_local)
  }
  _ = 1
}
// CHECK-TEXTUAL: "funcInlinableWithDefer()"
// CHECK-SIL-NO-SKIP: "funcInlinableWithDefer()"
// CHECK-SIL-SKIP-NONINLINE: "funcInlinableWithDefer()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcInlinableWithDefer()"

@inlinable public func funcInlinableWithNestedFuncAndTypealias() {
  let ALLNOTYPECHECK_outerLocal = "funcInlinableWithNestedFuncAndTypealias()"
  _blackHole(ALLNOTYPECHECK_outerLocal)

  typealias LocalType = Int
  func takesLocalType(_ x: LocalType) {
    let ALLNOTYPECHECK_innerLocal = "funcInlinableWithNestedFuncAndTypealias()@takesLocalType(_:)"
    _blackHole(ALLNOTYPECHECK_innerLocal)
  }
  takesLocalType(0)
}
// CHECK-TEXTUAL: "funcInlinableWithNestedFuncAndTypealias()"
// CHECK-SIL-NO-SKIP: "funcInlinableWithNestedFuncAndTypealias()"
// CHECK-SIL-SKIP-NONINLINE: "funcInlinableWithNestedFuncAndTypealias()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcInlinableWithNestedFuncAndTypealias()"

// CHECK-TEXTUAL: "funcInlinableWithNestedFuncAndTypealias()@takesLocalType(_:)"
// CHECK-SIL-NO-SKIP: "funcInlinableWithNestedFuncAndTypealias()@takesLocalType(_:)"
// CHECK-SIL-SKIP-NONINLINE: "funcInlinableWithNestedFuncAndTypealias()@takesLocalType(_:)"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcInlinableWithNestedFuncAndTypealias()@takesLocalType(_:)"

func funcInternal() {
  let NEVERTYPECHECK_local = "funcInternal()"
  _blackHole(NEVERTYPECHECK_local)
}
// CHECK-TEXTUAL-NOT: "funcInternal()"
// CHECK-SIL-NO-SKIP: "funcInternal()"
// CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "funcInternal()"

func funcPrivate() {
  let NEVERTYPECHECK_local = "funcPrivate()"
  _blackHole(NEVERTYPECHECK_local)
}
// CHECK-TEXTUAL-NOT: "funcPrivate()"
// CHECK-SIL-NO-SKIP: "funcPrivate()"
// CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "funcPrivate()"

func funcPublic() {
  let NEVERTYPECHECK_local = "funcPublic()"
  _blackHole(NEVERTYPECHECK_local)
}
// CHECK-TEXTUAL-NOT: "funcPublic()"
// CHECK-SIL-NO-SKIP: "funcPublic()"
// CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "funcPublic()"

func funcPublicWithDefer() {
  defer {
    let NEVERTYPECHECK_local = "funcPublicWithDefer()"
    _blackHole(NEVERTYPECHECK_local)
  }
  _ = 1
}
// CHECK-TEXTUAL-NOT: "funcPublicWithDefer()"
// CHECK-SIL-NO-SKIP: "funcPublicWithDefer()"
// CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "funcPublicWithDefer()"

public func funcPublicWithNestedFuncAndTypealias() {
  let INLINENOTYPECHECK_outerLocal = "funcPublicWithNestedFuncAndTypealias()"
  _blackHole(INLINENOTYPECHECK_outerLocal)

  typealias LocalType = Int
  func takesLocalType(_ x: LocalType) {
    let INLINENOTYPECHECK_innerLocal = "funcPublicWithNestedFuncAndTypealias()@takesLocalType(_:)"
    _blackHole(INLINENOTYPECHECK_innerLocal)
  }
  takesLocalType(0)
}
// CHECK-TEXTUAL-NOT: "funcPublicWithNestedFuncAndTypealias()"
// CHECK-SIL-NO-SKIP: "funcPublicWithNestedFuncAndTypealias()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcPublicWithNestedFuncAndTypealias()"
// CHECK-SIL-SKIP-NONINLINE-NOT: "funcPublicWithNestedFuncAndTypealias()"

// CHECK-TEXTUAL-NOT: "funcPublicWithNestedFuncAndTypealias()@takesLocalType(_:)"
// CHECK-SIL-NO-SKIP: "funcPublicWithNestedFuncAndTypealias()@takesLocalType(_:)"
// CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "funcPublicWithNestedFuncAndTypealias()@takesLocalType(_:)"

public func funcPublicWithNestedTypeClass() {
  let INLINENOTYPECHECK_local = "funcPublicWithNestedTypeClass()"
  _blackHole(INLINENOTYPECHECK_local)
  class C {}
}
// CHECK-TEXTUAL-NOT: "funcPublicWithNestedTypeClass()"
// CHECK-SIL-NO-SKIP: "funcPublicWithNestedTypeClass()"
// CHECK-SIL-SKIP-NONINLINE-NOT: "funcPublicWithNestedTypeClass()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcPublicWithNestedTypeClass()"

public func funcPublicWithNestedTypeEnum() {
  let INLINENOTYPECHECK_local = "funcPublicWithNestedTypeEnum()"
  _blackHole(INLINENOTYPECHECK_local)
  enum E {}
}
// CHECK-TEXTUAL-NOT: "funcPublicWithNestedTypeEnum()"
// CHECK-SIL-NO-SKIP: "funcPublicWithNestedTypeEnum()"
// CHECK-SIL-SKIP-NONINLINE-NOT: "funcPublicWithNestedTypeEnum()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcPublicWithNestedTypeEnum()"

public func funcPublicWithNestedTypealias() {
  let INLINENOTYPECHECK_local = "funcPublicWithNestedTypealias()"
  _blackHole(INLINENOTYPECHECK_local)
  typealias TA = Int
}
// CHECK-TEXTUAL-NOT: "funcPublicWithNestedTypealias()"
// CHECK-SIL-NO-SKIP: "funcPublicWithNestedTypealias()"
// CHECK-SIL-SKIP-NONINLINE-NOT: "funcPublicWithNestedTypealias()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcPublicWithNestedTypealias()"

public func funcPublicWithNestedTypeActor() {
  let INLINENOTYPECHECK_local = "funcPublicWithNestedTypeActor()"
  _blackHole(INLINENOTYPECHECK_local)
  actor A {}
}
// CHECK-TEXTUAL-NOT: "funcPublicWithNestedTypeActor()"
// CHECK-SIL-NO-SKIP: "funcPublicWithNestedTypeActor()"
// CHECK-SIL-SKIP-NONINLINE-NOT: "funcPublicWithNestedTypeActor()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcPublicWithNestedTypeActor()"

public func funcPublicWithNestedTypeProtocol() {
  let INLINENOTYPECHECK_local = "funcPublicWithNestedTypeProtocol()"
  _blackHole(INLINENOTYPECHECK_local)
  protocol P {}
}
// CHECK-TEXTUAL-NOT: "funcPublicWithNestedTypeProtocol()"
// CHECK-SIL-NO-SKIP: "funcPublicWithNestedTypeProtocol()"
// CHECK-SIL-SKIP-NONINLINE-NOT: "funcPublicWithNestedTypeProtocol()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcPublicWithNestedTypeProtocol()"

public func funcPublicWithNestedTypeStruct() {
  let INLINENOTYPECHECK_local = "funcPublicWithNestedTypeStruct()"
  _blackHole(INLINENOTYPECHECK_local)
  struct S {}
}
// CHECK-TEXTUAL-NOT: "funcPublicWithNestedTypeStruct()"
// CHECK-SIL-NO-SKIP: "funcPublicWithNestedTypeStruct()"
// CHECK-SIL-SKIP-NONINLINE-NOT: "funcPublicWithNestedTypeStruct()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcPublicWithNestedTypeStruct()"

public func funcPublicWithNestedTypeStructInNestedFunc() {
  let INLINENOTYPECHECK_local = "funcPublicWithNestedTypeStructInNestedFunc()"
  _blackHole(INLINENOTYPECHECK_local)

  func noType() {
    let NEVERTYPECHECK_innerLocal = "funcPublicWithNestedTypeStructInNestedFunc()@noType()"
    _blackHole(NEVERTYPECHECK_innerLocal)
  }

  func type() {
    let INLINENOTYPECHECK_innerLocal = "funcPublicWithNestedTypeStructInNestedFunc()@type()"
    _blackHole(INLINENOTYPECHECK_innerLocal)
    struct S {}
  }

}
// CHECK-TEXTUAL-NOT: "funcPublicWithNestedTypeStructInNestedFunc()"
// CHECK-SIL-NO-SKIP: "funcPublicWithNestedTypeStructInNestedFunc()"
// CHECK-SIL-SKIP-NONINLINE-NOT: "funcPublicWithNestedTypeStructInNestedFunc()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcPublicWithNestedTypeStructInNestedFunc()"

// CHECK-TEXTUAL-NOT: "funcPublicWithNestedTypeStructInNestedFunc()@noType()"
// CHECK-SIL-NO-SKIP: "funcPublicWithNestedTypeStructInNestedFunc()@noType()"
// CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "funcPublicWithNestedTypeStructInNestedFunc()@noType()"

// CHECK-TEXTUAL-NOT: "funcPublicWithNestedTypeStructInNestedFunc()@type()"
// CHECK-SIL-NO-SKIP: "funcPublicWithNestedTypeStructInNestedFunc()@type()"
// CHECK-SIL-SKIP-NONINLINE-NOT: "funcPublicWithNestedTypeStructInNestedFunc()@type()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcPublicWithNestedTypeStructInNestedFunc()@type()"

@_transparent public func funcTransparent() {
  let ALLNOTYPECHECK_local = "funcTransparent()"
  _blackHole(ALLNOTYPECHECK_local)
}
// CHECK-TEXTUAL: "funcTransparent()"
// CHECK-SIL-NO-SKIP: "funcTransparent()"
// CHECK-SIL-SKIP-NONINLINE: "funcTransparent()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcTransparent()"

@_transparent public func funcTransparentWithNestedFuncAndTypealias() {
  let ALLNOTYPECHECK_outerLocal = "funcTransparentWithNestedFuncAndTypealias()"
  _blackHole(ALLNOTYPECHECK_outerLocal)

  typealias LocalType = Int
  func takesLocalType(_ x: LocalType) {
    let ALLNOTYPECHECK_innerLocal = "funcTransparentWithNestedFuncAndTypealias()@takesLocalType(_:)"
    _blackHole(ALLNOTYPECHECK_innerLocal)
  }
  takesLocalType(0)
}
// CHECK-TEXTUAL: "funcTransparentWithNestedFuncAndTypealias()"
// CHECK-SIL-NO-SKIP: "funcTransparentWithNestedFuncAndTypealias()"
// CHECK-SIL-SKIP-NONINLINE: "funcTransparentWithNestedFuncAndTypealias()"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcTransparentWithNestedFuncAndTypealias()"

// CHECK-TEXTUAL: "funcTransparentWithNestedFuncAndTypealias()@takesLocalType(_:)"
// CHECK-SIL-NO-SKIP: "funcTransparentWithNestedFuncAndTypealias()@takesLocalType(_:)"
// CHECK-SIL-SKIP-NONINLINE: "funcTransparentWithNestedFuncAndTypealias()@takesLocalType(_:)"
// CHECK-SIL-SKIP-WITHOUTTYPES: "funcTransparentWithNestedFuncAndTypealias()@takesLocalType(_:)"

public struct Struct {
  public var didSetVar: Int = 1 {
    didSet {
      // This body is always typechecked.
      _blackHole("Struct.didSetVar.didSet")
    }
  }
  // CHECK-TEXTUAL-NOT: "Struct.didSetVar.didSet"
  // CHECK-SIL-NO-SKIP: "Struct.didSetVar.didSet"
  // CHECK-SIL-SKIP-NONINLINE: "Struct.didSetVar.didSet"
  // CHECK-SIL-SKIP-WITHOUTTYPES: "Struct.didSetVar.didSet"

  @inlinable public init(inlinable: Int) {
    let ALLNOTYPECHECK_local = "Struct.init(inlinable:)"
    _blackHole(ALLNOTYPECHECK_local)
  }
  // CHECK-TEXTUAL: "Struct.init(inlinable:)"
  // CHECK-SIL-NO-SKIP: "Struct.init(inlinable:)"
  // CHECK-SIL-SKIP-NONINLINE: "Struct.init(inlinable:)"
  // CHECK-SIL-SKIP-WITHOUTTYPES: "Struct.init(inlinable:)"

  @inline(__always) public init(inlineAlways: Int) {
    let NEVERTYPECHECK_local = "Struct.init(inlineAlways:)"
    _blackHole(NEVERTYPECHECK_local)
  }
  // CHECK-TEXTUAL-NOT: "Struct.init(inlineAlways:)"
  // CHECK-SIL-NO-SKIP: "Struct.init(inlineAlways:)"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.init(inlineAlways:)"

  init(internal: Int) {
    let NEVERTYPECHECK_local = "Struct.init(internal:)"
    _blackHole(NEVERTYPECHECK_local)
  }
  // CHECK-TEXTUAL-NOT: "Struct.init(internal:)"
  // CHECK-SIL-NO-SKIP: "Struct.init(internal:)"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.init(internal:)"

  private init(private: Int) {
    let NEVERTYPECHECK_local = "Struct.init(private:)"
    _blackHole(NEVERTYPECHECK_local)
  }
  // CHECK-TEXTUAL-NOT: "Struct.init(private:)"
  // CHECK-SIL-NO-SKIP: "Struct.init(private:)"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.init(private:)"

  public init(public: Int) {
    let NEVERTYPECHECK_local = "Struct.init(public:)"
    _blackHole(NEVERTYPECHECK_local)
  }
  // CHECK-TEXTUAL-NOT: "Struct.init(public:)"
  // CHECK-SIL-NO-SKIP: "Struct.init(public:)"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.init(public:)"

  @_transparent public init(transparent: Int) {
    let ALLNOTYPECHECK_local = "Struct.init(transparent:)"
    _blackHole(ALLNOTYPECHECK_local)
  }
  // CHECK-TEXTUAL: "Struct.init(transparent:)"
  // CHECK-SIL-NO-SKIP: "Struct.init(transparent:)"
  // CHECK-SIL-SKIP-NONINLINE: "Struct.init(transparent:)"
  // CHECK-SIL-SKIP-WITHOUTTYPES: "Struct.init(transparent:)"

  @inlinable public func inlinableFunc() {
    let ALLNOTYPECHECK_local = "Struct.inlinableFunc()"
    _blackHole(ALLNOTYPECHECK_local)
  }
  // CHECK-TEXTUAL: "Struct.inlinableFunc()"
  // CHECK-SIL-NO-SKIP: "Struct.inlinableFunc()"
  // CHECK-SIL-SKIP-NONINLINE: "Struct.inlinableFunc()"
  // CHECK-SIL-SKIP-WITHOUTTYPES: "Struct.inlinableFunc()"

  @inline(__always)
  public func inlineAlwaysFunc() {
    let NEVERTYPECHECK_local = "Struct.inlineAlwaysFunc()"
    _blackHole(NEVERTYPECHECK_local)
  }
  // CHECK-TEXTUAL-NOT: "Struct.inlineAlwaysFunc()"
  // CHECK-SIL-NO-SKIP: "Struct.inlineAlwaysFunc()"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.inlineAlwaysFunc()"

  public var inlineAlwaysSetter: Int {
    get { 0 }
    @inline(__always) set {
      let NEVERTYPECHECK_local = "Struct.inlineAlwaysSetter.setter"
      _blackHole(NEVERTYPECHECK_local)
    }
  }
  // CHECK-TEXTUAL-NOT: "Struct.inlineAlwaysSetter.setter"
  // CHECK-SIL-NO-SKIP: "Struct.inlineAlwaysSetter.setter"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.inlineAlwaysSetter.setter"

  @inline(__always) public var inlineAlwaysVar: Int {
    let NEVERTYPECHECK_local = "Struct.inlineAlwaysVar.getter"
    _blackHole(NEVERTYPECHECK_local)
    return 0
  }
  // CHECK-TEXTUAL-NOT: "Struct.inlineAlwaysVar.getter"
  // CHECK-SIL-NO-SKIP: "Struct.inlineAlwaysVar.getter"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.inlineAlwaysVar.getter"

  func internalFunc() {
    let NEVERTYPECHECK_local = "Struct.internalFunc()"
    _blackHole(NEVERTYPECHECK_local)
  }
  // CHECK-TEXTUAL-NOT: "Struct.internalFunc()"
  // CHECK-SIL-NO-SKIP: "Struct.internalFunc()"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.internalFunc()"

  private func privateFunc() {
    let NEVERTYPECHECK_local = "Struct.privateFunc()"
    _blackHole(NEVERTYPECHECK_local)
  }
  // CHECK-TEXTUAL-NOT: "Struct.privateFunc()"
  // CHECK-SIL-NO-SKIP: "Struct.privateFunc()"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.privateFunc()"

  public func publicFunc() {
    let NEVERTYPECHECK_local = "Struct.publicFunc()"
    _blackHole(NEVERTYPECHECK_local)
  }
  // CHECK-TEXTUAL-NOT: "Struct.publicFunc()"
  // CHECK-SIL-NO-SKIP: "Struct.publicFunc()"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.publicFunc()"

  @inlinable public subscript(inlinable: Int) -> Int {
    let ALLNOTYPECHECK_local = "Struct.subscript(inlinable:)"
    _blackHole(ALLNOTYPECHECK_local)
    return 0
  }
  // CHECK-TEXTUAL: "Struct.subscript(inlinable:)"
  // CHECK-SIL-NO-SKIP: "Struct.subscript(inlinable:)"
  // CHECK-SIL-SKIP-NONINLINE: "Struct.subscript(inlinable:)"
  // CHECK-SIL-SKIP-WITHOUTTYPES: "Struct.subscript(inlinable:)"

  @inline(__always) public subscript(inlineAlways: Int, _ x: Int) -> Int {
    let NEVERTYPECHECK_local = "Struct.subscript(inlineAlways:_:)"
    _blackHole(NEVERTYPECHECK_local)
    return 0
  }
  // CHECK-TEXTUAL-NOT: "Struct.subscript(inlineAlways:_:)"
  // CHECK-SIL-NO-SKIP: "Struct.subscript(inlineAlways:_:)"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.subscript(inlineAlways:_:)"

  subscript(internal: Int, _ x: Int, _ y: Int, _ z: Int) -> Int {
    let NEVERTYPECHECK_local = "Struct.subscript(internal:_:_:_:)"
    _blackHole(NEVERTYPECHECK_local)
    return 0
  }
  // CHECK-TEXTUAL-NOT: "Struct.subscript(internal:_:_:_:)"
  // CHECK-SIL-NO-SKIP: "Struct.subscript(internal:_:_:_:)"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.subscript(internal:_:_:_:)"

  private subscript(private: Int, _ x: Int, _ y: Int, _ z: Int, _ zz: Int, _ zzz: Int) -> Int {
    let NEVERTYPECHECK_local = "Struct.subscript(private:_:_:_:_:_:)"
    _blackHole(NEVERTYPECHECK_local)
    return 0
  }
  // CHECK-TEXTUAL-NOT: "Struct.subscript(private:_:_:_:_:_:)"
  // CHECK-SIL-NO-SKIP: "Struct.subscript(private:_:_:_:_:_:)"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.subscript(private:_:_:_:_:_:)"

  public subscript(public: Int, _ x: Int, _ y: Int, _ z: Int, _ zz: Int) -> Int {
    let NEVERTYPECHECK_local = "Struct.subscript(public:_:_:_:_:)"
    _blackHole(NEVERTYPECHECK_local)
    return 0
  }
  // CHECK-TEXTUAL-NOT: "Struct.subscript(public:_:_:_:_:)"
  // CHECK-SIL-NO-SKIP: "Struct.subscript(public:_:_:_:_:)"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.subscript(public:_:_:_:_:)"

  public subscript(transparent: Int, _ x: Int, _ y: Int) -> Int {
    @_transparent get {
      let ALLNOTYPECHECK_local = "Struct.subscript(transparent:_:_:)"
      _blackHole(ALLNOTYPECHECK_local)
      return 0
    }
  }
  // CHECK-TEXTUAL: "Struct.subscript(transparent:_:_:)"
  // CHECK-SIL-NO-SKIP: "Struct.subscript(transparent:_:_:)"
  // CHECK-SIL-SKIP-NONINLINE: "Struct.subscript(transparent:_:_:)"
  // CHECK-SIL-SKIP-WITHOUTTYPES: "Struct.subscript(transparent:_:_:)"

  @_transparent public func transparentFunc() {
    let ALLNOTYPECHECK_local = "Struct.transparentFunc()"
    _blackHole(ALLNOTYPECHECK_local)
  }
  // CHECK-TEXTUAL: "Struct.transparentFunc()"
  // CHECK-SIL-NO-SKIP: "Struct.transparentFunc()"
  // CHECK-SIL-SKIP-NONINLINE: "Struct.transparentFunc()"
  // CHECK-SIL-SKIP-WITHOUTTYPES: "Struct.transparentFunc()"

  @inlinable public var varWithInlinableGetter: Int {
    let ALLNOTYPECHECK_local = "Struct.varWithInlinableGetter.getter"
    _blackHole(ALLNOTYPECHECK_local)
    return 0
  }
  // CHECK-TEXTUAL: "Struct.varWithInlinableGetter.getter"
  // CHECK-SIL-NO-SKIP: "Struct.varWithInlinableGetter.getter"
  // CHECK-SIL-SKIP-NONINLINE: "Struct.varWithInlinableGetter.getter"
  // CHECK-SIL-SKIP-WITHOUTTYPES: "Struct.varWithInlinableGetter.getter"

  public var varWithInlinableSetter: Int {
    get { 0 }
    @inlinable set {
      let ALLNOTYPECHECK_local = "Struct.varWithInlinableSetter.setter"
      _blackHole(ALLNOTYPECHECK_local)
    }
  }
  // CHECK-TEXTUAL: "Struct.varWithInlinableSetter.setter"
  // CHECK-SIL-NO-SKIP: "Struct.varWithInlinableSetter.setter"
  // CHECK-SIL-SKIP-NONINLINE: "Struct.varWithInlinableSetter.setter"
  // CHECK-SIL-SKIP-WITHOUTTYPES: "Struct.varWithInlinableSetter.setter"

  public lazy var varWithLazyInitializer: Int = {
    // We currently don't have a way to skip typechecking a pattern binding
    // initializer expression
    _blackHole("Struct.varWithLazyInitializer.init")
    return 0
  }()
  // CHECK-TEXTUAL-NOT: "Struct.varWithLazyInitializer.init"
  // CHECK-SIL-NO-SKIP: "Struct.varWithLazyInitializer.init"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.varWithLazyInitializer.init"

  public var varWithObserverDidSet: Int = 1 {
    didSet {
      // Body typechecked regardless
      _blackHole("Struct.varWithObserverDidSet.didSet")
    }
  }
  // CHECK-TEXTUAL-NOT: "Struct.varWithObserverDidSet.didSet"
  // CHECK-SIL-NO-SKIP: "Struct.varWithObserverDidSet.didSet"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES: "Struct.varWithObserverDidSet.didSet"

  public var varWithObserverWillSet: Int = 1 {
    willSet {
      let ALLNOTYPECHECK_local = "Struct.varWithObserverWillSet.willSet"
      _blackHole(ALLNOTYPECHECK_local)
    }
  }
  // CHECK-TEXTUAL-NOT: "Struct.varWithObserverWillSet.willSet"
  // CHECK-SIL-NO-SKIP: "Struct.varWithObserverWillSet.willSet"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.varWithObserverWillSet.willSet"

  public var varWithPublicGetter: Int {
    let NEVERTYPECHECK_local = "Struct.varWithPublicGetter.getter"
    _blackHole(NEVERTYPECHECK_local)
    return 0
  }
  // CHECK-TEXTUAL-NOT: "Struct.varWithPublicGetter.getter"
  // CHECK-SIL-NO-SKIP: "Struct.varWithPublicGetter.getter"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.varWithPublicGetter.getter"

  public var varWithSetter: Int {
    get { 0 }
    set {
      let NEVERTYPECHECK_local = "Struct.varWithSetter.setter"
      _blackHole(NEVERTYPECHECK_local)
    }
  }
  // CHECK-TEXTUAL-NOT: "Struct.varWithSetter.setter"
  // CHECK-SIL-NO-SKIP: "Struct.varWithSetter.setter"
  // CHECK-SIL-SKIP-NONINLINE-OR-WITHOUTTYPES-NOT: "Struct.varWithSetter.setter"

  @_transparent public var varWithTransparentGetter: Int {
    let ALLNOTYPECHECK_local = "Struct.varWithTransparentGetter.getter"
    _blackHole(ALLNOTYPECHECK_local)
    return 0
  }
  // CHECK-TEXTUAL: "Struct.varWithTransparentGetter.getter"
  // CHECK-SIL-NO-SKIP: "Struct.varWithTransparentGetter.getter"
  // CHECK-SIL-SKIP-NONINLINE: "Struct.varWithTransparentGetter.getter"
  // CHECK-SIL-SKIP-WITHOUTTYPES: "Struct.varWithTransparentGetter.getter"
}
