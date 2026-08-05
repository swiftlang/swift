// Key paths formed in generic code are folded by `GenericSpecializer` once a
// specialized clone exists, so that IRGen can describe them as static objects.
//
// Specialization substitutes the `keypath` *instruction* but copies its pattern
// verbatim, leaving a clone whose substitutions are concrete while the pattern
// is still generic. That shape can't be emitted as data, so the pass folds the
// substitution map into the pattern. This mirrors what the mandatory pipeline
// already does for Embedded Swift, but here it is gated on `StaticKeyPaths`.

// RUN: %target-swift-frontend -emit-sil %s -O -parse-as-library -module-name spec -enable-experimental-feature StaticKeyPaths | %FileCheck -check-prefix=CHECK-SIL %s

// Without the feature the pass must not run: the clone keeps its generic
// pattern, exactly as before. Disable explicitly rather than relying on the
// default, so the check still means something when the whole suite is run with
// the feature forced on via SWIFT_TEST_OPTIONS.
// RUN: %target-swift-frontend -emit-sil %s -O -parse-as-library -module-name spec -disable-experimental-feature StaticKeyPaths | %FileCheck -check-prefix=CHECK-OFF %s

// RUN: %target-build-swift -O -parse-as-library %s -o %t-off.out -module-name main
// RUN: %target-run %t-off.out | %FileCheck -check-prefix=CHECK-OUT %s
// RUN: %target-build-swift -O -parse-as-library -Xfrontend -enable-experimental-feature -Xfrontend StaticKeyPaths %s -o %t-on.out -module-name main
// RUN: %target-run %t-on.out | %FileCheck -check-prefix=CHECK-OUT %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_StaticKeyPaths

public struct Box<T> {
  public var a: T
  public var b: T
  public var computed: T {
    get { a }
    set { a = newValue }
  }
}

@inline(never)
public func kpStored<T>() -> WritableKeyPath<Box<T>, T> { \Box<T>.a }

@inline(never)
public func kpComputed<T>() -> WritableKeyPath<Box<T>, T> { \Box<T>.computed }

// In the specialized clone the pattern is fully concrete: no `<τ_0_0>`
// substitution list after the closing paren, and the root is `Box<Int32>`
// rather than `Box<τ_0_0>`.
//
// CHECK-SIL-LABEL: sil {{.*}}@$s4spec8kpStored{{.*}}s5Int32V_Tg5 :
// CHECK-SIL:         keypath $WritableKeyPath<Box<Int32>, Int32>, (root $Box<Int32>; stored_property #Box.a : $Int32)

// A second instantiation folds independently.
//
// CHECK-SIL-LABEL: sil {{.*}}@$s4spec8kpStored{{.*}}s4Int8V_Tg5 :
// CHECK-SIL:         keypath $WritableKeyPath<Box<Int8>, Int8>, (root $Box<Int8>; stored_property #Box.a : $Int8)

// The unspecialized generic original is left alone -- it still carries a
// substitution list and a `τ_0_0`-rooted pattern, because there is nothing
// concrete to fold.
//
// CHECK-SIL-LABEL: sil {{.*}}@$s4spec8kpStored{{.*}}ylF :
// CHECK-SIL:         keypath $WritableKeyPath<Box<T>, T>, <τ_0_0> (root $Box<τ_0_0>;

// A computed component also picks up specialized accessor thunks. The `id`
// deliberately still names the generic accessor -- it is the component's
// identity, and carrying it across specialization unchanged is what keeps this
// key path equal to one written concretely (see the runtime check below).
//
// CHECK-SIL-LABEL: sil {{.*}}@$s4spec10kpComputed{{.*}}s5Int32V_Tg5 :
// CHECK-SIL:         keypath $WritableKeyPath<Box<Int32>, Int32>, (root $Box<Int32>; settable_property $Int32,
// CHECK-SIL-SAME:      getter @{{.*}}_Tg5 : $@convention(keypath_accessor_getter) (@in_guaranteed Box<Int32>) -> @out Int32
// CHECK-SIL-SAME:      setter @{{.*}}_Tg5 : $@convention(keypath_accessor_setter) (@in_guaranteed Int32, @inout Box<Int32>) -> ()

// With the feature off, even the specialized clone keeps its generic pattern.
//
// CHECK-OFF-LABEL: sil {{.*}}@$s4spec8kpStored{{.*}}s5Int32V_Tg5 :
// CHECK-OFF:         keypath $WritableKeyPath<Box<Int32>, Int32>, <τ_0_0> (root $Box<τ_0_0>;

@main
struct Main {
  static func main() {
    var box = Box<Int32>(a: 1, b: 2)

    // Reading and writing through a folded key path behaves identically.
    print(box[keyPath: kpStored()] == 1 ? "OK!" : "FAIL")      // CHECK-OUT: OK!
    box[keyPath: kpStored()] = 10
    print(box.a == 10 && box.b == 2 ? "OK!" : "FAIL")          // CHECK-OUT-NEXT: OK!

    print(box[keyPath: kpComputed()] == 10 ? "OK!" : "FAIL")   // CHECK-OUT-NEXT: OK!
    box[keyPath: kpComputed()] = 20
    print(box.a == 20 ? "OK!" : "FAIL")                        // CHECK-OUT-NEXT: OK!

    // A second instantiation must get its own specialization, not reuse the
    // first one's folded pattern.
    var small = Box<Int8>(a: 3, b: 4)
    small[keyPath: kpStored()] = 7
    print(small.a == 7 && small.b == 4 ? "OK!" : "FAIL")       // CHECK-OUT-NEXT: OK!

    // Folding must not change identity: a key path obtained from generic code
    // has to stay equal to the same one written concretely. This is why the
    // component `id` is carried across specialization unchanged.
    let viaGeneric: WritableKeyPath<Box<Int32>, Int32> = kpComputed()
    let direct = \Box<Int32>.computed
    print(viaGeneric == direct ? "OK!" : "FAIL")               // CHECK-OUT-NEXT: OK!
    print(viaGeneric.hashValue == direct.hashValue ? "OK!" : "FAIL")
    // CHECK-OUT-NEXT: OK!

    let storedViaGeneric: WritableKeyPath<Box<Int32>, Int32> = kpStored()
    print(storedViaGeneric == \Box<Int32>.a ? "OK!" : "FAIL")  // CHECK-OUT-NEXT: OK!

    // Distinct properties must still compare unequal.
    print(storedViaGeneric != \Box<Int32>.b ? "OK!" : "FAIL")  // CHECK-OUT-NEXT: OK!

    // The dynamic type survives folding.
    print(type(of: viaGeneric) == WritableKeyPath<Box<Int32>, Int32>.self ? "OK!" : "FAIL")
      // CHECK-OUT-NEXT: OK!
  }
}
