// RUN: %target-swift-emit-silgen %s -verify -swift-version 5 | %FileCheck %s

protocol P {
  var p: P { get set }
  var q: P? { get set }
  func p(_: P)
  func q(_: P)
}

struct S : P {
  var p: P
  var q: P?
  func p(_: P) {}
  func q(_: P) {}
}

class Base : P {
  var p: P
  var q: P?
  func p(_: P) {}
  func q(_: P) {}
  init(_ p: P) { self.p = p }
}

class Derived : Base {
}

func genericOverload<T>(_: T) {}
func genericOverload<T>(_: T?) {}
func genericOptional<T>(_: T?) {}
func genericNoOptional<T>(_: T) {}

// CHECK-LABEL: sil hidden @$s7ranking22propertyVersusFunctionyyAA1P_p_xtAaCRzlF
func propertyVersusFunction<T : P>(_ p: P, _ t: T) {
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  let _ = p.p
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  let _: P = p.p
  // CHECK: function_ref @$s7ranking1PP1pyyAaB_pFTc
  let _: (P) -> () = p.p
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  let _: P? = p.p
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  let _: Any = p.p
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  let _: Any? = p.p

  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  // CHECK: function_ref @$s7ranking15genericOverloadyyxlF
  genericOverload(p.p)
  // CHECK: witness_method $@opened("{{.*}}") P, #P.q!getter.1
  // CHECK: function_ref @$s7ranking15genericOverloadyyxSglF
  genericOverload(p.q)
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  // CHECK: function_ref @$s7ranking15genericOptionalyyxSglF
  genericOptional(p.p)
  // CHECK: witness_method $@opened("{{.*}}") P, #P.q!getter.1
  // CHECK: function_ref @$s7ranking15genericOptionalyyxSglF
  genericOptional(p.q)
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  // CHECK: function_ref @$s7ranking17genericNoOptionalyyxlF
  genericNoOptional(p.p)
  // CHECK: witness_method $@opened("{{.*}}") P, #P.q!getter.1
  // CHECK: function_ref @$s7ranking17genericNoOptionalyyxlF
  genericNoOptional(p.q)

  // CHECK: witness_method $T, #P.p!getter.1
  let _ = t.p
  // CHECK: witness_method $T, #P.p!getter.1
  let _: P = t.p
  // CHECK: function_ref @$s7ranking1PP1pyyAaB_pFTc
  let _: (P) -> () = t.p
  // CHECK: witness_method $T, #P.p!getter.1
  let _: P? = t.p
  // CHECK: witness_method $T, #P.p!getter.1
  let _: Any = t.p
  // CHECK: witness_method $T, #P.p!getter.1
  let _: Any? = t.p

  // CHECK: witness_method $T, #P.p!getter.1
  // CHECK: function_ref @$s7ranking15genericOverloadyyxlF
  genericOverload(t.p)
  // CHECK: witness_method $T, #P.q!getter.1
  // CHECK: function_ref @$s7ranking15genericOverloadyyxSglF
  genericOverload(t.q)
  // CHECK: witness_method $T, #P.p!getter.1
  // CHECK: function_ref @$s7ranking15genericOptionalyyxSglF
  genericOptional(t.p)
  // CHECK: witness_method $T, #P.q!getter.1
  // CHECK: function_ref @$s7ranking15genericOptionalyyxSglF
  genericOptional(t.q)
  // CHECK: witness_method $T, #P.p!getter.1
  // CHECK: function_ref @$s7ranking17genericNoOptionalyyxlF
  genericNoOptional(t.p)
  // CHECK: witness_method $T, #P.q!getter.1
  // CHECK: function_ref @$s7ranking17genericNoOptionalyyxlF
  genericNoOptional(t.q)
}

extension P {
  func propertyVersusFunction() {
    // CHECK: witness_method $Self, #P.p!getter.1
    let _ = self.p
    // CHECK: witness_method $Self, #P.p!getter.1
    let _: P = self.p
    // CHECK: function_ref @$s7ranking1PP1pyyAaB_pFTc
    let _: (P) -> () = self.p
    // CHECK: witness_method $Self, #P.p!getter.1
    let _: P? = self.p
    // CHECK: witness_method $Self, #P.p!getter.1
    let _: Any = self.p
    // CHECK: witness_method $Self, #P.p!getter.1
    let _: Any? = self.p

    // CHECK: witness_method $Self, #P.p!getter.1
    // CHECK: function_ref @$s7ranking15genericOverloadyyxlF
    genericOverload(self.p)
    // CHECK: witness_method $Self, #P.q!getter.1
    // CHECK: function_ref @$s7ranking15genericOverloadyyxSglF
    genericOverload(self.q)
    // CHECK: witness_method $Self, #P.p!getter.1
    // CHECK: function_ref @$s7ranking15genericOptionalyyxSglF
    genericOptional(self.p)
    // CHECK: witness_method $Self, #P.q!getter.1
    // CHECK: function_ref @$s7ranking15genericOptionalyyxSglF
    genericOptional(self.q)
    // CHECK: witness_method $Self, #P.p!getter.1
    // CHECK: function_ref @$s7ranking17genericNoOptionalyyxlF
    genericNoOptional(self.p)
    // CHECK: witness_method $Self, #P.q!getter.1
    // CHECK: function_ref @$s7ranking17genericNoOptionalyyxlF
    genericNoOptional(self.q)
  }
}

//--------------------------------------------------------------------

func f0<T>(_ x: T) {}

// FIXME: Lookup breaks if these come after f1!
class A { 
  init() {} 
};
class B : A { 
  override init() { super.init() } 
}

func f1(_ a: A) -> A { return a }
func f1(_ b: B) -> B { return b }

func testDerived(b: B) {
  // CHECK-LABEL: sil hidden @$s7ranking11testDerived1byAA1BC_tF
  // CHECK: function_ref @$s7ranking2f1yAA1BCADF
  // CHECK: function_ref @$s7ranking2f0yyxlF
  f0(f1(b))
  // CHECK: end sil function '$s7ranking11testDerived1byAA1BC_tF'
}

protocol X {
  var foo: Int { get }
  var bar: Int { get }
  func baz() -> Int
  subscript(foo: String) -> Int { get }
}

class Y {
  var foo: Int = 0
  func baz() -> Int { return foo }
  subscript(foo: String) -> Int { return 0 }
}
extension Y {
  var bar: Int { return foo }
}

protocol Z : Y {
  var foo: Int { get }
  var bar: Int { get }
  func baz() -> Int
  subscript(foo: String) -> Int { get }
}

class GenericClass<T> {
  var foo: T
  init(_ foo: T) { self.foo = foo }
  func baz() -> T { return foo }
}
extension GenericClass {
  var bar: T { return foo }
  subscript(foo: String) -> Int { return 0 }
}

// Make sure we favour the class implementation over the protocol requirement.

// CHECK-LABEL: sil hidden @$s7ranking32testGenericPropertyProtocolClassyyxAA1YCRbzAA1XRzlF
func testGenericPropertyProtocolClass<T : X & Y>(_ t: T) {
  _ = t.foo   // CHECK: class_method {{%.*}} : $Y, #Y.foo!getter.1
  _ = t.bar   // CHECK: function_ref @$s7ranking1YC3barSivg
  _ = t.baz() // CHECK: class_method {{%.*}} : $Y, #Y.baz
  _ = t[""]   // CHECK: class_method {{%.*}} : $Y, #Y.subscript!getter.1
}

// CHECK-LABEL: sil hidden @$s7ranking36testExistentialPropertyProtocolClassyyAA1X_AA1YCXcF
func testExistentialPropertyProtocolClass(_ t: X & Y) {
  _ = t.foo   // CHECK: class_method {{%.*}} : $Y, #Y.foo!getter.1
  _ = t.bar   // CHECK: function_ref @$s7ranking1YC3barSivg
  _ = t.baz() // CHECK: class_method {{%.*}} : $Y, #Y.baz
  _ = t[""]   // CHECK: class_method {{%.*}} : $Y, #Y.subscript!getter.1
}

// CHECK-LABEL: sil hidden @$s7ranking46testGenericPropertySubclassConstrainedProtocolyyxAA1ZRzlF
func testGenericPropertySubclassConstrainedProtocol<T : Z>(_ t: T) {
  _ = t.foo   // CHECK: class_method {{%.*}} : $Y, #Y.foo!getter.1
  _ = t.bar   // CHECK: function_ref @$s7ranking1YC3barSivg
  _ = t.baz() // CHECK: class_method {{%.*}} : $Y, #Y.baz
  _ = t[""]   // CHECK: class_method {{%.*}} : $Y, #Y.subscript!getter.1
}

// CHECK-LABEL: sil hidden @$s7ranking50testExistentialPropertySubclassConstrainedProtocolyyAA1Z_pF
func testExistentialPropertySubclassConstrainedProtocol(_ t: Z) {
  _ = t.foo   // CHECK: class_method {{%.*}} : $Y, #Y.foo!getter.1
  _ = t.bar   // CHECK: function_ref @$s7ranking1YC3barSivg
  _ = t.baz() // CHECK: class_method {{%.*}} : $Y, #Y.baz
  _ = t[""]   // CHECK: class_method {{%.*}} : $Y, #Y.subscript!getter.1
}

// CHECK-LABEL: sil hidden @$s7ranking43testExistentialPropertyProtocolGenericClassyyAA1X_AA0fG0CySiGXcF
func testExistentialPropertyProtocolGenericClass(_ t: GenericClass<Int> & X) {
  _ = t.foo   // CHECK: class_method {{%.*}} : $GenericClass<Int>, #GenericClass.foo!getter.1
  _ = t.bar   // CHECK: function_ref @$s7ranking12GenericClassC3barxvg
  _ = t.baz() // CHECK: class_method {{%.*}} : $GenericClass<Int>, #GenericClass.baz
  _ = t[""]   // CHECK: function_ref @$s7ranking12GenericClassCySiSScig
}

// CHECK-LABEL: sil hidden @$s7ranking43testExistentialPropertyProtocolGenericClassyyAA1X_AA0fG0CySSGXcF
func testExistentialPropertyProtocolGenericClass(_ t: GenericClass<String> & X) {
  _ = t.foo   // CHECK: class_method {{%.*}} : $GenericClass<String>, #GenericClass.foo!getter.1
  _ = t.bar   // CHECK: function_ref @$s7ranking12GenericClassC3barxvg
  _ = t.baz() // CHECK: class_method {{%.*}} : $GenericClass<String>, #GenericClass.baz
  _ = t[""]   // CHECK: function_ref @$s7ranking12GenericClassCySiSScig
}

extension X where Self : Y {
  // CHECK-LABEL: sil hidden @$s7ranking1XPA2A1YCRbzrlE32testGenericPropertyProtocolClassyyxF
  func testGenericPropertyProtocolClass(_ x: Self) {
    _ = self.foo   // CHECK: class_method {{%.*}} : $Y, #Y.foo!getter.1
    _ = self.bar   // CHECK: function_ref @$s7ranking1YC3barSivg
    _ = self.baz() // CHECK: class_method {{%.*}} : $Y, #Y.baz
    _ = self[""]   // CHECK: class_method {{%.*}} : $Y, #Y.subscript!getter.1
  }
}

extension X where Self : GenericClass<Int> {
  // CHECK-LABEL: sil hidden @$s7ranking1XPA2A12GenericClassCySiGRbzrlE04testb16PropertyProtocolbC0yyxF
  func testGenericPropertyProtocolGenericClass(_ x: Self) {
    _ = self.foo   // CHECK: class_method {{%.*}} : $GenericClass<Int>, #GenericClass.foo!getter.1
    _ = self.bar   // CHECK: function_ref @$s7ranking12GenericClassC3barxvg
    _ = self.baz() // CHECK: class_method {{%.*}} : $GenericClass<Int>, #GenericClass.baz
    _ = self[""]   // CHECK: function_ref @$s7ranking12GenericClassCySiSScig
  }
}

extension X where Self : GenericClass<String> {
  // CHECK-LABEL: sil hidden @$s7ranking1XPA2A12GenericClassCySSGRbzrlE04testb16PropertyProtocolbC0yyxF
  func testGenericPropertyProtocolGenericClass(_ x: Self) {
    _ = self.foo   // CHECK: class_method {{%.*}} : $GenericClass<String>, #GenericClass.foo!getter.1
    _ = self.bar   // CHECK: function_ref @$s7ranking12GenericClassC3barxvg
    _ = self.baz() // CHECK: class_method {{%.*}} : $GenericClass<String>, #GenericClass.baz
    _ = self[""]   // CHECK: function_ref @$s7ranking12GenericClassCySiSScig
  }
}

//--------------------------------------------------------------------
// Pointer conversions
//--------------------------------------------------------------------

struct UnsafePointerStruct {
  // CHECK-LABEL: sil hidden @$s7ranking19UnsafePointerStructVyACSPyxGSgclufC : $@convention(method) <U> (Optional<UnsafePointer<U>>, @thin UnsafePointerStruct.Type) -> UnsafePointerStruct
  init<U>(_ from: UnsafePointer<U>) {}
  init<U>(_ from: UnsafePointer<U>?) {
    // CHECK: function_ref @$s7ranking19UnsafePointerStructVyACSPyxGclufC : $@convention(method) <τ_0_0> (UnsafePointer<τ_0_0>, @thin UnsafePointerStruct.Type) -> UnsafePointerStruct
    self.init(from!)
  }
}

// CHECK-LABEL: sil hidden @$s7ranking22useUnsafePointerStructyySPyxGlF : $@convention(thin) <U> (UnsafePointer<U>) -> ()
func useUnsafePointerStruct<U>(_ ptr: UnsafePointer<U>) {
  // CHECK: function_ref @$s7ranking19UnsafePointerStructVyACSPyxGclufC : $@convention(method) <τ_0_0> (UnsafePointer<τ_0_0>, @thin UnsafePointerStruct.Type) -> UnsafePointerStruct
  let _: UnsafePointerStruct = UnsafePointerStruct(ptr)
}
