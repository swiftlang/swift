// RUN: %target-swift-emit-irgen -O                          \
// RUN:     -enable-experimental-feature NoncopyableGenerics \
// RUN:     -disable-type-layout                             \
// RUN:     %s                                               \
// RUN: |                                                    \
// RUN: %IRGenFileCheck %s

@_silgen_name("external_symbol")
func external_symbol()

public class C<T> {
  deinit {
    external_symbol()
  }
}

public struct InnerDeinitingReleasableNC<T> : ~Copyable {
  public let i1: Int
  public let i2: Int
  public let i3: Int
  public let i4: Int
  public let c1: C<T>
  public let c2: C<T>
  public let c3: C<T>
  public let c4: C<T>

  deinit {
    external_symbol()
  }
}

public struct InnerDeinitingDestructableNC<T> : ~Copyable {
  public let t: T
  public let i1: Int
  public let i2: Int
  public let i3: Int
  public let i4: Int
  public let c1: C<T>
  public let c2: C<T>
  public let c3: C<T>
  public let c4: C<T>

  deinit {
    external_symbol()
  }
}

public struct OuterDeinitingNC_1<T> : ~Copyable {
  public let i1: Int
  public let c1: C<T>
  public let i: InnerDeinitingReleasableNC<T>
  deinit {
    external_symbol()
  }
}

public struct OuterNC_1<T> : ~Copyable {
  public let i1: Int
  public let c1: C<T>
  public let i: InnerDeinitingReleasableNC<T>
}

public struct OuterNC_2<T> : ~Copyable {
  public let i1: Int
  public let c1: C<T>
  public let i: InnerDeinitingDestructableNC<T>
}

// Destroyed value:
// - has deinit
// On lifetime end:
// - call deinit
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions22takeOuterDeinitingNC_1yyAA0efG2_1VyxGnlF"(
// CHECK-SAME:      ptr{{.*}} %0,
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK:         [[RESPONSE:%[^,]+]] = call{{.*}} @"$s24moveonly_value_functions18OuterDeinitingNC_1VMa"(
//           :        i64 0,
// CHECK-SAME:        ptr %T)
// CHECK:         [[OUTER_DEINITING_NC_1_METADATA:%[^,]+]] = extractvalue %swift.metadata_response [[RESPONSE]]
// CHECK:         call{{.*}} @"$s24moveonly_value_functions18OuterDeinitingNC_1VfD"(
// CHECK-SAME:        ptr [[OUTER_DEINITING_NC_1_METADATA]],
// CHECK-SAME:        ptr{{.*}} %0)
// CHECK:       }
public func takeOuterDeinitingNC_1<T>(_ t: consuming OuterDeinitingNC_1<T>) {
  external_symbol()
}

// If the destroyed value has no deinit, is releasable, and contains a
// noncopyable value with a deinit, call the outlined release function.
// Destroyed value:
// - has NO deinit
// - contains value type with deinit
// - is releasable
// On lifetime end:
// - call outlined release function
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions13takeOuterNC_1yyAA0eF2_1VyxGnlF"(
// CHECK-SAME:      ptr{{.*}} %0,
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK:         [[RESPONSE:%[^,]+]] = call{{.*}} @"$s24moveonly_value_functions26InnerDeinitingReleasableNCVMa"(
//           :        i64 0,
// CHECK-SAME:        ptr %T)
// CHECK:         [[INNER_DEINITING_RELEASABLE_NC_METADATA:%[^,]+]] = extractvalue %swift.metadata_response [[RESPONSE]]
// CHECK:         call{{.*}} @"$s24moveonly_value_functions9OuterNC_1VyxGlWOs"(
// CHECK-SAME:        ptr %0,
// CHECK-SAME:        ptr [[INNER_DEINITING_RELEASABLE_NC_METADATA]])
// CHECK:       }

// Verify that the outlined release function takes the metadata for the
// move-only-with-deinit type InnerDeinitingReleasableNC<T> and passes it along
// to that deinit.
// $s24moveonly_value_functions9OuterNC_1VyxGlWOs ---> outlined release of moveonly_value_functions.OuterNC_2<A>
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions9OuterNC_1VyxGlWOs"(
// CHECK-SAME:      ptr %0,
// CHECK-SAME:      ptr %"InnerDeinitingReleasableNC<T>")
// CHECK-SAME:  {
//                ...
//                ...
// CHECK:         call swiftcc void @"$s24moveonly_value_functions26InnerDeinitingReleasableNCVfD"(
// CHECK-SAME:        ptr %"InnerDeinitingReleasableNC<T>",
//           :        ptr noalias nocapture swiftself dereferenceable(64) %deinit.arg)
// CHECK:       }
public func takeOuterNC_1<T>(_ o: consuming OuterNC_1<T>) {
  external_symbol()
}

// If the destroyed value has no deinit, is releasable, and contains a
// noncopyable value with a deinit, call the outlined destroy function.
// Destroyed value:
// - has NO deinit
// - contains value type with deinit
// - is NOT releasable
// On lifetime end:
// - call outlined destroy destroy
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions13takeOuterNC_2yyAA0eF2_2VyxGnlF"(
// CHECK-SAME:      ptr{{.*}} %0,
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK:         [[RESPONSE:%[^,]+]] = call{{.*}} @"$s24moveonly_value_functions28InnerDeinitingDestructableNCVMa"(
// CHECK-SAME:        [[INT]] 0,
// CHECK-SAME:        ptr %T)
// CHECK:         [[INNER_DEINITING_DESTRUCTABLE_NC_METADATA:%[^,]+]] = extractvalue %swift.metadata_response [[RESPONSE]]
// CHECK:         call{{.*}} @"$s24moveonly_value_functions9OuterNC_2VyxGlWOh"(
// CHECK-SAME:        ptr %0,
// CHECK-SAME:        ptr %T,
// CHECK-SAME:        ptr [[INNER_DEINITING_DESTRUCTABLE_NC_METADATA]],
//           :        ptr %4)
// CHECK:       }

// Verify that the outlined destroy function takes the metadata for the
// move-only-with-deinit type InnerDeinitingDestructable<T> and passes it along
// to that deinit.
// $s24moveonly_value_functions9OuterNC_2VyxGlWOh ---> outlined destroy of moveonly_value_functions.OuterNC_2<A>
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions9OuterNC_2VyxGlWOh"(
// CHECK-SAME:      ptr %0, 
// CHECK-SAME:      ptr %T, 
// CHECK-SAME:      ptr %"InnerDeinitingDestructableNC<T>", 
// CHECK-SAME:      ptr %"OuterNC_2<T>")
// CHECK-SAME:  {
//                ...
//                ...
// CHECK:         call{{.*}} @"$s24moveonly_value_functions28InnerDeinitingDestructableNCVfD"(
// CHECK-SAME:        ptr %"InnerDeinitingDestructableNC<T>", 
//           :        ptr noalias swiftself %3)
// CHECK:       }
public func takeOuterNC_2<T>(_ o: consuming OuterNC_2<T>) {
  external_symbol()
}
