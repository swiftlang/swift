// Key paths formed in a generic context and then specialized: the pattern's
// accessor thunks start out generic, and `specializeKeyPathInst` folds the
// instruction's substitution map into the pattern so IRGen can emit a static
// instance. See `KeyPathInst::getStaticInstanceClassType`.

// RUN: %target-swift-emit-sil %s -module-name kpgc -enable-experimental-feature Embedded -wmo -o - | %FileCheck -check-prefix=CHECK-SIL %s

// The capturing key paths below hash their captured index, which pulls in the
// hash-seed initializer and so `arc4random_buf`. Ubuntu 22.04's glibc predates
// that, so link through `%target-embedded-link` -- it injects the local RNG
// shim on Linux -- rather than `%target-run-simple-swift`, which would cost
// this test its Linux coverage the way `keypaths-hashable.swift` had to.
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -O -enable-experimental-feature Embedded -wmo -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck -check-prefix=CHECK-OUT %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public struct G<T> {
  public var stored: T
  public var computedT: T {
    get { stored }
    set { stored = newValue }
  }
  public init(stored: T) { self.stored = stored }
}

public struct Wrapper<T> {
  public var g: G<T>
  public init(g: G<T>) { self.g = g }
}

// A subscript with captured indices, in a generic type. The captured index is
// concretely `Int`, but its equals/hash thunks are still generic over the
// *pattern's* signature, so they need specializing along with the accessors.
public struct Pair<T> {
  public var a: T
  public var b: T
  public subscript(i: Int) -> T {
    get { i == 0 ? a : b }
    set { if i == 0 { a = newValue } else { b = newValue } }
  }
  public init(a: T, b: T) { self.a = a; self.b = b }
}

// A generic subscript in a non-generic type, where the captured value's own
// type is the generic parameter.
public struct Box {
  public var v: Int32
  public subscript<K: Hashable>(k: K) -> Int32 {
    get { v }
    set { v = newValue }
  }
  public init(v: Int32) { self.v = v }
}

// A key path to a computed property of a generic type, formed generically.
@inline(never)
public func kpComputed<T>(_: T.Type) -> WritableKeyPath<G<T>, T> {
  return \G<T>.computedT
}

// A multi-component chain whose tail is a generic computed property.
@inline(never)
public func kpChain<T>(_: T.Type) -> WritableKeyPath<Wrapper<T>, T> {
  return \Wrapper<T>.g.computedT
}

@inline(never)
public func kpPair<T>(_ i: Int) -> WritableKeyPath<Pair<T>, T> {
  return \Pair<T>[i]
}

@inline(never)
public func kpBox<K: Hashable>(_ k: K) -> WritableKeyPath<Box, Int32> {
  return \Box[k]
}

// Keep the two `kpBox` operands from being folded together, so `==` below has
// to compare the captured values rather than take its object-identity fast path.
@inline(never)
public func opaque(_ x: Int) -> Int { x }

// After specialization the pattern must carry no substitution list (there is no
// trailing `<Int32>` after the closing paren) and must reference concrete,
// specialized accessor thunks.
// CHECK-SIL-LABEL: sil {{.*}}@$e4kpgc10kpComputed{{.*}}Ttg5 :
// CHECK-SIL:         keypath $WritableKeyPath<G<Int32>, Int32>, (root $G<Int32>;
// CHECK-SIL-SAME:      getter @{{.*}}_Tg5 : $@convention(keypath_accessor_getter) (@in_guaranteed G<Int32>) -> @out Int32
// CHECK-SIL-SAME:      setter @{{.*}}_Tg5 : $@convention(keypath_accessor_setter) (@in_guaranteed Int32, @inout G<Int32>) -> ())

// Same for the chain, whose last component is the generic computed property.
// CHECK-SIL-LABEL: sil {{.*}}@$e4kpgc7kpChain{{.*}}Ttg5 :
// CHECK-SIL:         keypath $WritableKeyPath<Wrapper<Int32>, Int32>, (root $Wrapper<Int32>; stored_property #Wrapper.g : $G<Int32>;
// CHECK-SIL-SAME:      getter @{{.*}}_Tg5 : $@convention(keypath_accessor_getter) (@in_guaranteed G<Int32>) -> @out Int32
// CHECK-SIL-SAME:      setter @{{.*}}_Tg5 : $@convention(keypath_accessor_setter) (@in_guaranteed Int32, @inout G<Int32>) -> ())

// A capturing component specializes its equals/hash thunks too, not just the
// getter and setter. For `Pair` the captured index is already `Int`, so this
// only passes if the thunks are specialized for the *pattern's* signature.
// CHECK-SIL-LABEL: sil {{.*}}@$e4kpgc6kpPair{{.*}}s5Int32V_Tg5 :
// CHECK-SIL:         keypath $WritableKeyPath<Pair<Int32>, Int32>, (root $Pair<Int32>;
// CHECK-SIL-SAME:      getter @{{.*}}_Tg5 : $@convention(keypath_accessor_getter) (@in_guaranteed Pair<Int32>, @in_guaranteed Int) -> @out Int32
// CHECK-SIL-SAME:      indices [%$0 : $Int : $Int]
// CHECK-SIL-SAME:      indices_equals @{{.*}}_Tg5 : $@convention(keypath_accessor_equals) (@in_guaranteed Int, @in_guaranteed Int) -> Bool
// CHECK-SIL-SAME:      indices_hash @{{.*}}_Tg5 : $@convention(keypath_accessor_hash) (@in_guaranteed Int) -> Int

// For the generic subscript the captured value's own type is substituted too:
// the index goes from `$τ_0_0 : $*τ_0_0` to `$Int : $*Int`.
// CHECK-SIL-LABEL: sil {{.*}}@$e4kpgc5kpBox{{.*}}Si_Tg5 :
// CHECK-SIL:         keypath $WritableKeyPath<Box, Int32>, (root $Box;
// CHECK-SIL-SAME:      getter @{{.*}}_Tg5 : $@convention(keypath_accessor_getter) (@in_guaranteed Box, @in_guaranteed Int) -> @out Int32
// CHECK-SIL-SAME:      indices [%$0 : $Int : $*Int]
// CHECK-SIL-SAME:      indices_equals @{{.*}}_Tg5 : $@convention(keypath_accessor_equals) (@in_guaranteed Int, @in_guaranteed Int) -> Bool
// CHECK-SIL-SAME:      indices_hash @{{.*}}_Tg5 : $@convention(keypath_accessor_hash) (@in_guaranteed Int) -> Int

var g = G<Int32>(stored: 41)
let kp = kpComputed(Int32.self)
print(g[keyPath: kp] == 41 ? "OK!" : "FAIL") // CHECK-OUT: OK!
g[keyPath: kp] = 99
print(g.stored == 99 ? "OK!" : "FAIL") // CHECK-OUT: OK!

var w = Wrapper(g: G<Int32>(stored: 7))
let chain = kpChain(Int32.self)
print(w[keyPath: chain] == 7 ? "OK!" : "FAIL") // CHECK-OUT: OK!
w[keyPath: chain] = 8
print(w.g.stored == 8 ? "OK!" : "FAIL") // CHECK-OUT: OK!

// Captured index in a generic type, at two different instantiations.
var p = Pair<Int32>(a: 10, b: 20)
print(p[keyPath: kpPair(1)] == 20 ? "OK!" : "FAIL") // CHECK-OUT: OK!
p[keyPath: kpPair(0)] = 99
print(p.a == 99 && p.b == 20 ? "OK!" : "FAIL") // CHECK-OUT: OK!
var p8 = Pair<Int8>(a: 1, b: 2)
p8[keyPath: kpPair(1)] = 7
print(p8.b == 7 ? "OK!" : "FAIL") // CHECK-OUT: OK!

// Generic subscript, capturing a value of the generic parameter type.
var box = Box(v: 5)
print(box[keyPath: kpBox(42)] == 5 ? "OK!" : "FAIL") // CHECK-OUT: OK!
box[keyPath: kpBox(42)] = 8
print(box.v == 8 ? "OK!" : "FAIL") // CHECK-OUT: OK!

// Equality runs the specialized equals thunk: these are distinct allocations,
// so the comparison can't be short-circuited by object identity.
print(kpBox(opaque(3)) == kpBox(opaque(3)) ? "OK!" : "FAIL") // CHECK-OUT: OK!
print(kpBox(opaque(3)) != kpBox(opaque(4)) ? "OK!" : "FAIL") // CHECK-OUT: OK!
