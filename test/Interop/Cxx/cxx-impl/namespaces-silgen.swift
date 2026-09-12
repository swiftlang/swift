// Verifies how a `@cxx @implementation` function in an extension of an
// imported C++ namespace is lowered. A C++ namespace member imports as a
// static member of the namespace enum, so the lowered SIL type drops the
// formal `@thin` metatype `self`, and the prolog materializes that `self`
// instead of claiming a SIL argument for it.

// RUN: %target-swift-emit-silgen \
// RUN:   -module-name main \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s

// REQUIRES: swift_feature_CxxImplementation

import Namespaces


extension Outer {
  // An ordinary Swift static method, it keeps the `@thin Outer.Type` self.
  // CHECK-LABEL: sil hidden [ossa] @$sSo5OuterO4mainE11plainHelperys5Int32VAFFZ : $@convention(method) (Int32, @thin Outer.Type) -> Int32 {
  // CHECK:       bb0(%0 : $Int32, [[SELF:%.*]] : $@thin Outer.Type):
  // CHECK:         debug_value [[SELF]], let, name "self", argno 2
  static func plainHelper(_ x: Int32) -> Int32 { return x * 3 }

  // int Outer::add(int a, int b);
  // CHECK-LABEL: sil [asmname "{{.*}}add{{.*}}"] [ossa] @$sSo5OuterO4mainE3addys5Int32VAF_AFtFZTo : $@convention(c) (Int32, Int32) -> Int32 {
  // CHECK:       bb0(%0 : $Int32, %1 : $Int32):
  // CHECK:         [[SELF:%.*]] = metatype $@thin Outer.Type
  // CHECK:         debug_value [[SELF]], let, name "self", argno 3
  @cxx @implementation
  public static func add(_ a: Int32, _ b: Int32) -> Int32 { return a + b }

  // void Outer::voidNoArgs();
  // CHECK-LABEL: sil [asmname "{{.*}}voidNoArgs{{.*}}"] [ossa] @$sSo5OuterO4mainE10voidNoArgsyyFZTo : $@convention(c) () -> () {
  // CHECK:       bb0:
  // CHECK:         [[SELF:%.*]] = metatype $@thin Outer.Type
  // CHECK:         debug_value [[SELF]], let, name "self", argno 1
  @cxx @implementation
  public static func voidNoArgs() {}

  // int Outer::renamedTarget(int x);
  // CHECK-LABEL: sil [asmname "{{.*}}renamedTarget{{.*}}"] [ossa] @$sSo5OuterO4mainE12swiftRenamedys5Int32VAFFZTo : $@convention(c) (Int32) -> Int32 {
  // CHECK:       bb0(%0 : $Int32):
  // CHECK:         [[SELF:%.*]] = metatype $@thin Outer.Type
  // CHECK:         debug_value [[SELF]], let, name "self", argno 2
  @cxx(renamedTarget) @implementation
  public static func swiftRenamed(_ x: Int32) -> Int32 { return x * 2 }

  // int Outer::overloadedByArity(int x);
  // CHECK-LABEL: sil [asmname "{{.*}}overloadedByArity{{.*}}"] [ossa] @$sSo5OuterO4mainE17overloadedByArityys5Int32VAFFZTo : $@convention(c) (Int32) -> Int32 {
  // CHECK:       bb0(%0 : $Int32):
  // CHECK:         [[SELF:%.*]] = metatype $@thin Outer.Type
  // CHECK:         debug_value [[SELF]], let, name "self", argno 2
  // CHECK:         [[HELPER:%.*]] = function_ref @$sSo5OuterO4mainE11plainHelperys5Int32VAFFZ : $@convention(method) (Int32, @thin Outer.Type) -> Int32
  // CHECK:         apply [[HELPER]](%0, [[SELF]]) : $@convention(method) (Int32, @thin Outer.Type) -> Int32
  @cxx @implementation
  public static func overloadedByArity(_ x: Int32) -> Int32 {
    return self.plainHelper(x)
  }

  // int Outer::overloadedByArity(int x, int y);
  // CHECK-LABEL: sil [asmname "{{.*}}overloadedByArity{{.*}}"] [ossa] @$sSo5OuterO4mainE17overloadedByArityys5Int32VAF_AFtFZTo : $@convention(c) (Int32, Int32) -> Int32 {
  // CHECK:       bb0(%0 : $Int32, %1 : $Int32):
  // CHECK:         [[SELF:%.*]] = metatype $@thin Outer.Type
  // CHECK:         debug_value [[SELF]], let, name "self", argno 3
  // CHECK:         [[HELPER:%.*]] = function_ref @$sSo5OuterO4mainE11plainHelperys5Int32VAFFZ : $@convention(method) (Int32, @thin Outer.Type) -> Int32
  // CHECK:         apply [[HELPER]](%0, [[SELF]]) : $@convention(method) (Int32, @thin Outer.Type) -> Int32
  @cxx @implementation
  public static func overloadedByArity(_ x: Int32, _ y: Int32) -> Int32 {
    return plainHelper(x) + y
  }
}

extension Outer.Inner {
  // int Outer::Inner::nestedFunc(int x);
  // CHECK-LABEL: sil [asmname "{{.*}}nestedFunc{{.*}}"] [ossa] @$sSo5OuterO5InnerO4mainE10nestedFuncys5Int32VAHFZTo : $@convention(c) (Int32) -> Int32 {
  // CHECK:       bb0(%0 : $Int32):
  // CHECK:         [[SELF:%.*]] = metatype $@thin Outer.Inner.Type
  // CHECK:         debug_value [[SELF]], let, name "self", argno 2
  @cxx @implementation
  public static func nestedFunc(_ x: Int32) -> Int32 { return x }
}


// Swift-side calls

// CHECK-LABEL: sil [ossa] @$s4main18callNamespaceFuncss5Int32VyF : $@convention(thin) () -> Int32 {
// CHECK:         [[VOID_NO_ARGS:%.*]] = function_ref @$sSo5OuterO10voidNoArgsyyFZTo : $@convention(c) () -> ()
// CHECK:         apply [[VOID_NO_ARGS]]() : $@convention(c) () -> ()
// CHECK:         [[ADD:%.*]] = function_ref @$sSo5OuterO3addys5Int32VAE_AEtFZTo : $@convention(c) (Int32, Int32) -> Int32
// CHECK:         apply [[ADD]]({{.*}}) : $@convention(c) (Int32, Int32) -> Int32
// CHECK:         [[RENAMED:%.*]] = function_ref @$sSo5OuterO13renamedTargetys5Int32VAEFZTo : $@convention(c) (Int32) -> Int32
// CHECK:         apply [[RENAMED]]({{.*}}) : $@convention(c) (Int32) -> Int32
// CHECK:         [[NESTED:%.*]] = function_ref @$sSo5OuterO5InnerO10nestedFuncys5Int32VAGFZTo : $@convention(c) (Int32) -> Int32
// CHECK:         apply [[NESTED]]({{.*}}) : $@convention(c) (Int32) -> Int32
public func callNamespaceFuncs() -> Int32 {
  Outer.voidNoArgs()
  return Outer.add(1, 2) + Outer.renamedTarget(4) + Outer.Inner.nestedFunc(3)
}

// CHECK-LABEL: sil [asmname "{{.*}}voidNoArgs{{.*}}"] @$sSo5OuterO10voidNoArgsyyFZTo : $@convention(c) () -> ()
// CHECK-LABEL: sil [asmname "{{.*}}add{{.*}}"] @$sSo5OuterO3addys5Int32VAE_AEtFZTo : $@convention(c) (Int32, Int32) -> Int32
// CHECK-LABEL: sil [asmname "{{.*}}renamedTarget{{.*}}"] @$sSo5OuterO13renamedTargetys5Int32VAEFZTo : $@convention(c) (Int32) -> Int32
// CHECK-LABEL: sil [asmname "{{.*}}nestedFunc{{.*}}"] @$sSo5OuterO5InnerO10nestedFuncys5Int32VAGFZTo : $@convention(c) (Int32) -> Int32
