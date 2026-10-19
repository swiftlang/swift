// Verifies how a `@cxx @implementation` method in an extension of an imported
// C++ struct is lowered. The lowered SIL type of a static method drops the
// formal `@thin` metatype `self` and the prolog materializes it. An instance
// method is lowered with the C++ method convention: `self` is the last SIL
// parameter and is passed indirectly, `@in_guaranteed` for a `const` method
// and `@inout` for a `mutating` one, so that the entry point receives it as
// C++'s `this`.

// RUN: %target-swift-emit-silgen \
// RUN:   -module-name main \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s

// REQUIRES: swift_feature_CxxImplementation

import Methods


extension Counter {
  // An ordinary Swift static method, it keeps the `@thin Counter.Type` self.
  // CHECK-LABEL: sil hidden [ossa] @$sSo7CounterV4mainE11plainHelperys5Int32VAFFZ : $@convention(method) (Int32, @thin Counter.Type) -> Int32 {
  // CHECK:       bb0(%0 : $Int32, [[SELF:%.*]] : $@thin Counter.Type):
  // CHECK:         debug_value [[SELF]], let, name "self", argno 2
  static func plainHelper(_ x: Int32) -> Int32 { return x * 3 }

  // An ordinary Swift instance method, it takes `self` by value.
  // CHECK-LABEL: sil hidden [ossa] @$sSo7CounterV4mainE19plainInstanceHelpers5Int32VyF : $@convention(method) (Counter) -> Int32 {
  // CHECK:       bb0([[SELF:%.*]] : $Counter):
  // CHECK:         debug_value [[SELF]], let, name "self", argno 1
  func plainInstanceHelper() -> Int32 { return value }

  // static Counter Counter::make(int v);
  // CHECK-LABEL: sil [asmname "{{.*}}make{{.*}}"] [ossa] @$sSo7CounterV4mainE4makeyABs5Int32VFZTo : $@convention(c) (Int32) -> Counter {
  // CHECK:       bb0(%0 : $Int32):
  // CHECK:         [[SELF:%.*]] = metatype $@thin Counter.Type
  // CHECK:         debug_value [[SELF]], let, name "self", argno 2
  // CHECK:         [[HELPER:%.*]] = function_ref @$sSo7CounterV4mainE11plainHelperys5Int32VAFFZ : $@convention(method) (Int32, @thin Counter.Type) -> Int32
  // CHECK:         apply [[HELPER]](%0, [[SELF]]) : $@convention(method) (Int32, @thin Counter.Type) -> Int32
  @cxx @implementation
  public static func make(_ v: Int32) -> Counter {
    return Counter(value: self.plainHelper(v))
  }

  // int Counter::get() const;
  // CHECK-LABEL: sil [asmname "{{.*}}get{{.*}}"] [ossa] @$sSo7CounterV4mainE3gets5Int32VyFTo : $@convention(cxx_method) (@in_guaranteed Counter) -> Int32 {
  // CHECK:       bb0([[SELF_ADDR:%.*]] : $*Counter):
  // CHECK:         [[SELF:%.*]] = load [trivial] [[SELF_ADDR]]
  // CHECK:         debug_value [[SELF]], let, name "self", argno 1
  @cxx @implementation
  public func get() -> Int32 { return value }

  // void Counter::add(int d);
  // CHECK-LABEL: sil [asmname "{{.*}}add{{.*}}"] [ossa] @$sSo7CounterV4mainE3addyys5Int32VFTo : $@convention(cxx_method) (Int32, @inout Counter) -> () {
  // CHECK:       bb0(%0 : $Int32, [[SELF:%.*]] : $*Counter):
  // CHECK:         debug_value [[SELF]], var, name "self", argno 2, expr op_deref
  // CHECK:         begin_access [modify] [unknown] [[SELF]]
  @cxx @implementation
  public mutating func add(_ d: Int32) { value += d }

  // int Counter::overloadedByArity() const;
  // CHECK-LABEL: sil [asmname "{{.*}}overloadedByArity{{.*}}"] [ossa] @$sSo7CounterV4mainE17overloadedByAritys5Int32VyFTo : $@convention(cxx_method) (@in_guaranteed Counter) -> Int32 {
  // CHECK:       bb0([[SELF_ADDR:%.*]] : $*Counter):
  // CHECK:         [[SELF:%.*]] = load [trivial] [[SELF_ADDR]]
  // CHECK:         debug_value [[SELF]], let, name "self", argno 1
  @cxx @implementation
  public func overloadedByArity() -> Int32 { return value }

  // int Counter::overloadedByArity(int x) const;
  // CHECK-LABEL: sil [asmname "{{.*}}overloadedByArity{{.*}}"] [ossa] @$sSo7CounterV4mainE17overloadedByArityys5Int32VAFFTo : $@convention(cxx_method) (Int32, @in_guaranteed Counter) -> Int32 {
  // CHECK:       bb0(%0 : $Int32, [[SELF_ADDR:%.*]] : $*Counter):
  // CHECK:         [[SELF:%.*]] = load [trivial] [[SELF_ADDR]]
  // CHECK:         debug_value [[SELF]], let, name "self", argno 2
  // CHECK:         [[HELPER:%.*]] = function_ref @$sSo7CounterV4mainE19plainInstanceHelpers5Int32VyF : $@convention(method) (Counter) -> Int32
  // CHECK:         apply [[HELPER]]([[SELF]]) : $@convention(method) (Counter) -> Int32
  @cxx @implementation
  public func overloadedByArity(_ x: Int32) -> Int32 {
    return plainInstanceHelper() + x
  }

  // int Counter::renamedTarget() const;
  // CHECK-LABEL: sil [asmname "{{.*}}renamedTarget{{.*}}"] [ossa] @$sSo7CounterV4mainE12swiftRenameds5Int32VyFTo : $@convention(cxx_method) (@in_guaranteed Counter) -> Int32 {
  // CHECK:       bb0([[SELF_ADDR:%.*]] : $*Counter):
  // CHECK:         [[SELF:%.*]] = load [trivial] [[SELF_ADDR]]
  // CHECK:         debug_value [[SELF]], let, name "self", argno 1
  @cxx(renamedTarget) @implementation
  public func swiftRenamed() -> Int32 { return value }
}


// The const and the non-const overload are told apart by `mutating`, which
// selects the `self` convention. The non-const overload keeps the importer's
// `Mutating` suffix in its Swift name but is emitted under the C++ symbol.

extension Pair {
  // int Pair::adjust(int x) const;
  // CHECK-LABEL: sil [asmname "{{.*}}adjust{{.*}}"] [ossa] @$sSo4PairV4mainE6adjustys5Int32VAFFTo : $@convention(cxx_method) (Int32, @in_guaranteed Pair) -> Int32 {
  // CHECK:       bb0(%0 : $Int32, [[SELF_ADDR:%.*]] : $*Pair):
  // CHECK:         [[SELF:%.*]] = load [trivial] [[SELF_ADDR]]
  // CHECK:         debug_value [[SELF]], let, name "self", argno 2
  @cxx @implementation
  public func adjust(_ x: Int32) -> Int32 { return value + x }

  // int Pair::adjust(int x);
  // CHECK-LABEL: sil [asmname "{{.*}}adjust{{.*}}"] [ossa] @$sSo4PairV4mainE14adjustMutatingys5Int32VAFFTo : $@convention(cxx_method) (Int32, @inout Pair) -> Int32 {
  // CHECK:       bb0(%0 : $Int32, [[SELF:%.*]] : $*Pair):
  // CHECK:         debug_value [[SELF]], var, name "self", argno 2, expr op_deref
  // CHECK:         begin_access [modify] [unknown] [[SELF]]
  @cxx(adjust) @implementation
  public mutating func adjustMutating(_ x: Int32) -> Int32 { value += x; return value }

  // int Pair::adjust(int x, int y);
  // CHECK-LABEL: sil [asmname "{{.*}}adjust{{.*}}"] [ossa] @$sSo4PairV4mainE6adjustys5Int32VAF_AFtFTo : $@convention(cxx_method) (Int32, Int32, @inout Pair) -> Int32 {
  // CHECK:       bb0(%0 : $Int32, %1 : $Int32, [[SELF:%.*]] : $*Pair):
  // CHECK:         debug_value [[SELF]], var, name "self", argno 3, expr op_deref
  @cxx @implementation
  public mutating func adjust(_ x: Int32, _ y: Int32) -> Int32 { value += x + y; return value }
}


// A result too large for registers is still a direct SIL result; the indirect
// return is an IRGen matter.

extension Holder {
  // Triple Holder::spread(int k) const;
  // CHECK-LABEL: sil [asmname "{{.*}}spread{{.*}}"] [ossa] @$sSo6HolderV4mainE6spreadySo6TripleVs5Int32VFTo : $@convention(cxx_method) (Int32, @in_guaranteed Holder) -> Triple {
  // CHECK:       bb0(%0 : $Int32, [[SELF_ADDR:%.*]] : $*Holder):
  // CHECK:         [[SELF:%.*]] = load [trivial] [[SELF_ADDR]]
  // CHECK:         debug_value [[SELF]], let, name "self", argno 2
  @cxx @implementation
  public func spread(_ k: Int32) -> Triple {
    return Triple(a: CLongLong(value), b: CLongLong(k), c: CLongLong(value + k))
  }
}


// A non-trivial receiver is address-only, so `self` stays an address in the
// body.

extension NonTrivialReceiver {
  // int NonTrivialReceiver::read() const;
  // CHECK-LABEL: sil [asmname "{{.*}}read{{.*}}"] [ossa] @$sSo18NonTrivialReceiverV4mainE4reads5Int32VyFTo : $@convention(cxx_method) (@in_guaranteed NonTrivialReceiver) -> Int32 {
  // CHECK:       bb0([[SELF:%.*]] : $*NonTrivialReceiver):
  // CHECK:         debug_value [[SELF]], let, name "self", argno 1, expr op_deref
  // CHECK:         struct_element_addr [[SELF]], #NonTrivialReceiver.value
  @cxx @implementation
  public func read() -> Int32 { return value }

  // void NonTrivialReceiver::write(int v);
  // CHECK-LABEL: sil [asmname "{{.*}}write{{.*}}"] [ossa] @$sSo18NonTrivialReceiverV4mainE5writeyys5Int32VFTo : $@convention(cxx_method) (Int32, @inout NonTrivialReceiver) -> () {
  // CHECK:       bb0(%0 : $Int32, [[SELF:%.*]] : $*NonTrivialReceiver):
  // CHECK:         debug_value [[SELF]], var, name "self", argno 2, expr op_deref
  // CHECK:         begin_access [modify] [unknown] [[SELF]]
  @cxx @implementation
  public mutating func write(_ v: Int32) { value = v }
}


// A static method of a foreign reference type drops and materializes its
// metatype `self` like a struct's.

@available(SwiftStdlib 5.8, *)
extension Widget {
  // static int Widget::count();
  // CHECK-LABEL: sil {{.*}}[asmname "{{.*}}count{{.*}}"] [ossa] @$sSo6WidgetV4mainE5counts5Int32VyFZTo : $@convention(c) () -> Int32 {
  // CHECK:       bb0:
  // CHECK:         [[SELF:%.*]] = metatype $@thin Widget.Type
  // CHECK:         debug_value [[SELF]], let, name "self", argno 1
  @cxx @implementation
  public static func count() -> Int32 { return 0 }
}


// Swift-side calls

// CHECK-LABEL: sil [ossa] @$s4main11callMethodsys5Int32VSo7CounterVz_So4PairVzSo6HolderVztF : $@convention(thin) (@inout Counter, @inout Pair, @inout Holder) -> Int32 {
// CHECK:       bb0([[C:%.*]] : $*Counter, [[P:%.*]] : $*Pair, [[H:%.*]] : $*Holder):
// CHECK:         [[MAKE:%.*]] = function_ref @$sSo7CounterV4makeyABs5Int32VFZTo : $@convention(c) (Int32) -> Counter
// CHECK:         apply [[MAKE]]({{.*}}) : $@convention(c) (Int32) -> Counter
// CHECK:         [[C_MODIFY:%.*]] = begin_access [modify] [unknown] [[C]]
// CHECK:         [[ADD:%.*]] = function_ref @$sSo7CounterV3addyys5Int32VFTo : $@convention(cxx_method) (Int32, @inout Counter) -> ()
// CHECK:         apply [[ADD]]({{.*}}, [[C_MODIFY]]) : $@convention(cxx_method) (Int32, @inout Counter) -> ()
// CHECK:         [[C_READ:%.*]] = begin_access [read] [static] [[C]]
// CHECK:         [[GET:%.*]] = function_ref @$sSo7CounterV3gets5Int32VyFTo : $@convention(cxx_method) (@in_guaranteed Counter) -> Int32
// CHECK:         apply [[GET]]([[C_READ]]) : $@convention(cxx_method) (@in_guaranteed Counter) -> Int32
// CHECK:         [[RENAMED:%.*]] = function_ref @$sSo7CounterV13renamedTargets5Int32VyFTo : $@convention(cxx_method) (@in_guaranteed Counter) -> Int32
// CHECK:         apply [[RENAMED]]({{.*}}) : $@convention(cxx_method) (@in_guaranteed Counter) -> Int32
// CHECK:         [[P_READ:%.*]] = begin_access [read] [static] [[P]]
// CHECK:         [[ADJUST:%.*]] = function_ref @$sSo4PairV6adjustys5Int32VAEFTo : $@convention(cxx_method) (Int32, @in_guaranteed Pair) -> Int32
// CHECK:         apply [[ADJUST]]({{.*}}, [[P_READ]]) : $@convention(cxx_method) (Int32, @in_guaranteed Pair) -> Int32
// CHECK:         [[P_MODIFY:%.*]] = begin_access [modify] [unknown] [[P]]
// CHECK:         [[ADJUST_MUTATING:%.*]] = function_ref @$sSo4PairV14adjustMutatingys5Int32VAEFTo : $@convention(cxx_method) (Int32, @inout Pair) -> Int32
// CHECK:         apply [[ADJUST_MUTATING]]({{.*}}, [[P_MODIFY]]) : $@convention(cxx_method) (Int32, @inout Pair) -> Int32
// CHECK:         [[H_READ:%.*]] = begin_access [read] [static] [[H]]
// CHECK:         [[SPREAD:%.*]] = function_ref @$sSo6HolderV6spreadySo6TripleVs5Int32VFTo : $@convention(cxx_method) (Int32, @in_guaranteed Holder) -> Triple
// CHECK:         apply [[SPREAD]]({{.*}}, [[H_READ]]) : $@convention(cxx_method) (Int32, @in_guaranteed Holder) -> Triple
public func callMethods(_ c: inout Counter, _ p: inout Pair, _ h: inout Holder) -> Int32 {
  var result = Counter.make(1).value
  c.add(2)
  result += c.get() + c.renamedTarget()
  result += p.adjust(4) + p.adjustMutating(5)
  result += Int32(h.spread(8).a)
  return result
}

// CHECK-LABEL: sil [asmname "{{.*}}make{{.*}}"] @$sSo7CounterV4makeyABs5Int32VFZTo : $@convention(c) (Int32) -> Counter
// CHECK-LABEL: sil [asmname "{{.*}}add{{.*}}"] @$sSo7CounterV3addyys5Int32VFTo : $@convention(cxx_method) (Int32, @inout Counter) -> ()
// CHECK-LABEL: sil [asmname "{{.*}}get{{.*}}"] @$sSo7CounterV3gets5Int32VyFTo : $@convention(cxx_method) (@in_guaranteed Counter) -> Int32
// CHECK-LABEL: sil [asmname "{{.*}}renamedTarget{{.*}}"] @$sSo7CounterV13renamedTargets5Int32VyFTo : $@convention(cxx_method) (@in_guaranteed Counter) -> Int32
// CHECK-LABEL: sil [asmname "{{.*}}adjust{{.*}}"] @$sSo4PairV6adjustys5Int32VAEFTo : $@convention(cxx_method) (Int32, @in_guaranteed Pair) -> Int32
// CHECK-LABEL: sil [asmname "{{.*}}adjust{{.*}}"] @$sSo4PairV14adjustMutatingys5Int32VAEFTo : $@convention(cxx_method) (Int32, @inout Pair) -> Int32
// CHECK-LABEL: sil [asmname "{{.*}}spread{{.*}}"] @$sSo6HolderV6spreadySo6TripleVs5Int32VFTo : $@convention(cxx_method) (Int32, @in_guaranteed Holder) -> Triple

// CHECK-LABEL: sil {{.*}}[ossa] @$s4main10callWidgets5Int32VyF : $@convention(thin) () -> Int32 {
// CHECK:         [[COUNT:%.*]] = function_ref @$sSo6WidgetV5counts5Int32VyFZTo : $@convention(c) () -> Int32
// CHECK:         apply [[COUNT]]() : $@convention(c) () -> Int32
@available(SwiftStdlib 5.8, *)
public func callWidget() -> Int32 { return Widget.count() }

// CHECK-LABEL: sil {{.*}}[asmname "{{.*}}count{{.*}}"] @$sSo6WidgetV5counts5Int32VyFZTo : $@convention(c) () -> Int32
