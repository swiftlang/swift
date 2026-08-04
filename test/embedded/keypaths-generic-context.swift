// Key paths formed in a generic context and then specialized: the pattern's
// accessor thunks start out generic, and `specializeKeyPathInst` folds the
// instruction's substitution map into the pattern so IRGen can emit a static
// instance. See `KeyPathInst::getStaticInstanceClassType`.

// RUN: %target-swift-emit-sil %s -module-name kpgc -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedKeyPaths -wmo -o - | %FileCheck -check-prefix=CHECK-SIL %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedKeyPaths -wmo -runtime-compatibility-version none %target-embedded-posix-shim) | %FileCheck -check-prefix=CHECK-OUT %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedKeyPaths
// Embedded key paths and SIL opaque values don't currently mix: the
// combination trips `getSILArgumentConvention`. `keypaths-static.swift` and
// `keypaths-exec.swift` carry the same XFAIL.
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

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
