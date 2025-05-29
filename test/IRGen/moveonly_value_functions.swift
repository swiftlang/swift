// RUN: %target-swift-emit-irgen -O                          \
// RUN:     -Xllvm -sil-disable-pass=deinit-devirtualizer    \
// RUN:     -disable-type-layout                             \
// RUN:     %s                                               \
// RUN: |                                                    \
// RUN: %IRGenFileCheck %s

@_silgen_name("external_symbol")
func external_symbol()

public protocol P {
  static func foo()
}

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

public struct InnerDeinitingWithLayoutNC<T> : ~Copyable {
  public let t: T
  deinit {}
}

public struct InnerDeinitingWithoutLayoutNC<T>: ~Copyable {
  public let ptr: Int
  deinit {}
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

public struct GenericContext_1<T> {
}

extension GenericContext_1 : P where T : P {
  public static func foo() {
    T.foo()
  }
  public struct OuterNC_1: ~Copyable {
    let i: Inner_NC1

    func doit() {
      i.beinit()
    }
  }

  public struct Inner_NC1: ~Copyable {
    let t: T
    let ptr: Int
    deinit {
      T.foo()
    }
    func beinit() {}
  }
}

public enum OuterSinglePayloadNC_1<T>: ~Copyable {
  case none
  case some(InnerDeinitingWithoutLayoutNC<T>)
}

public enum OuterSinglePayloadNC_2<T>: ~Copyable {
  case none
  case some(InnerDeinitingReleasableNC<T>)
}

public enum OuterSinglePayloadNC_3<T>: ~Copyable {
  case none
  case some(InnerDeinitingDestructableNC<T>)
}

public enum OuterMultiPayloadNC_1<T>: ~Copyable {
  case none
  case some(InnerDeinitingWithLayoutNC<T>)
  case some2(InnerDeinitingWithLayoutNC<T>)
}

public enum OuterMultiPayloadNC_2<T>: ~Copyable {
  case none
  case some(InnerDeinitingWithoutLayoutNC<T>)
  case some2(InnerDeinitingWithoutLayoutNC<T>)
}

// FIXME: Uncomment.
public enum OuterMultiPayloadNC_3<T>: ~Copyable {
  case none
  case some(InnerDeinitingReleasableNC<T>)
  case some2(InnerDeinitingReleasableNC<T>)
}

public enum OuterMultiPayloadNC_4<T>: ~Copyable {
  case none
  case some(InnerDeinitingDestructableNC<T>)
  case some2(InnerDeinitingDestructableNC<T>)
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
// CHECK:         call{{.*}} @"$s24moveonly_value_functions9OuterNC_1VyxGlWOh"(
// CHECK-SAME:        ptr %0,
// CHECK-SAME:        ptr %T)
// CHECK:       }

// Verify that the outlined release function takes the metadata for the
// move-only-with-deinit type InnerDeinitingReleasableNC<T> and passes it along
// to that deinit.
// $s24moveonly_value_functions9OuterNC_1VyxGlWOh ---> outlined destroy of moveonly_value_functions.OuterNC_2<A>
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions9OuterNC_1VyxGlWOh"(
// CHECK-SAME:      ptr %0,
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
//                ...
//                ...
// CHECK:         [[RESPONSE:%[^,]+]] = call{{.*}} @"$s24moveonly_value_functions26InnerDeinitingReleasableNCVMa"(
//           :        i64 0,
// CHECK-SAME:        ptr %T)
// CHECK:         [[INNER_DEINITING_RELEASABLE_NC_METADATA:%[^,]+]] = extractvalue %swift.metadata_response [[RESPONSE]]
// CHECK:         call swiftcc void @"$s24moveonly_value_functions26InnerDeinitingReleasableNCVfD"(
// CHECK-SAME:        ptr [[INNER_DEINITING_RELEASABLE_NC_METADATA]],
//           :        ptr noalias{{( nocapture)?}} swiftself{{( captures\(none\))?}} dereferenceable(64) %deinit.arg)
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
// CHECK-NOT:     @"$s24moveonly_value_functions28InnerDeinitingDestructableNCVMa"
// CHECK-NOT:     @"$s24moveonly_value_functions9OuterNC_2VMa"
// CHECK:         call{{.*}} @"$s24moveonly_value_functions9OuterNC_2VyxGlWOh"(
// CHECK-SAME:        ptr %0,
// CHECK-SAME:        ptr %T)
// CHECK:       }

// Verify that the outlined destroy function takes the metadata for the
// move-only-with-deinit type InnerDeinitingDestructable<T> and passes it along
// to that deinit.
// $s24moveonly_value_functions9OuterNC_2VyxGlWOh ---> outlined destroy of moveonly_value_functions.OuterNC_2<A>
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions9OuterNC_2VyxGlWOh"(
// CHECK-SAME:      ptr %0, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
//                ...
//                ...
// CHECK:         [[RESPONSE:%[^,]+]] = call{{.*}} @"$s24moveonly_value_functions28InnerDeinitingDestructableNCVMa"(
//           :        [[INT]] 0,
// CHECK-SAME:        ptr %T)
// CHECK:         [[INNER_DEINITING_DESTRUCTABLE_NC_METADATA:%[^,]+]] = extractvalue %swift.metadata_response [[RESPONSE]]
// CHECK:         call{{.*}} @"$s24moveonly_value_functions28InnerDeinitingDestructableNCVfD"(
// CHECK-SAME:        ptr [[INNER_DEINITING_DESTRUCTABLE_NC_METADATA]], 
//           :        ptr noalias swiftself %3)
// CHECK:       }
public func takeOuterNC_2<T>(_ o: consuming OuterNC_2<T>) {
  external_symbol()
}

// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions29takeGenericContext_1OuterNC_1yyAA0eF2_1VA2A1PRzlE0gH2_1Vyx_GnAaERzlF"(
// CHECK-SAME:      ptr noalias %0, 
// CHECK-SAME:      ptr %T, 
// CHECK-SAME:      ptr %T.P)
// CHECK-SAME:  {
// CHECK-NOT:     @"$s24moveonly_value_functions16GenericContext_1VA2A1PRzlE9Inner_NC1VMa"
// CHECK-NOT:     @"$s24moveonly_value_functions16GenericContext_1VA2A1PRzlE9OuterNC_1VMa"
// CHECK:         call ptr @"$s24moveonly_value_functions16GenericContext_1VA2A1PRzlE9OuterNC_1Vyx_GAaDRzlWOh"(
// CHECK-SAME:        ptr %0, 
// CHECK-SAME:        ptr %T, 
// CHECK-SAME:        ptr %T.P)
// CHECK:       }
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions16GenericContext_1VA2A1PRzlE9OuterNC_1Vyx_GAaDRzlWOh"(
// CHECK-SAME:      ptr %0, 
// CHECK-SAME:      ptr %T, 
// CHECK-SAME:      ptr %T.P)
// CHECK-SAME:  {
// CHECK:         [[RESPONSE:%[^,]+]] = call{{.*}} @"$s24moveonly_value_functions16GenericContext_1VA2A1PRzlE9Inner_NC1VMa"(
//           :        i64 0, 
// CHECK-SAME:        ptr %T, 
// CHECK-SAME:        ptr %T.P)
// CHECK:         [[GENERIC_CONTEXT_1_INNER_NC_1_METADATA:%[^,]+]] = extractvalue %swift.metadata_response [[RESPONSE]], 0
// CHECK:         call swiftcc void @"$s24moveonly_value_functions16GenericContext_1VA2A1PRzlE9Inner_NC1VfD"(
// CHECK-SAME:        ptr [[GENERIC_CONTEXT_1_INNER_NC_1_METADATA]], 
// CHECK-SAME:        ptr noalias swiftself %0)
// CHECK:       }
public func takeGenericContext_1OuterNC_1<T : P>(_ e: consuming GenericContext_1<T>.OuterNC_1) {}

// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions26takeOuterSinglePayloadNC_1yyAA0efgH2_1OyxGnlF"(
//           :      i64 %0, 
//           :      i8 %1, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK:         call{{.*}} @"$s24moveonly_value_functions22OuterSinglePayloadNC_1OyxGlWOe"(
//           :        i64 %0, 
//           :        i1 %2, 
// CHECK-SAME:        ptr %T)
// CHECK:       }
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions22OuterSinglePayloadNC_1OyxGlWOe"(
//           :      i64 %0, 
//           :      i1 %1, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK:         call{{.*}} @"$s24moveonly_value_functions29InnerDeinitingWithoutLayoutNCVfD"(
//           :        i64 %0,
// CHECK-SAME:        ptr %T)
// CHECK:       }
public func takeOuterSinglePayloadNC_1<T>(_ e: consuming OuterSinglePayloadNC_1<T>) {}

// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions26takeOuterSinglePayloadNC_2yyAA0efgH2_2OyxGnlF"(
//           :      ptr noalias {{(nocapture|captures\(none\))}} dereferenceable(64) %0, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK:         call{{.*}} @"$s24moveonly_value_functions22OuterSinglePayloadNC_2OyxGlWOh"(
// CHECK-SAME:        ptr %0, 
// CHECK-SAME:        ptr %T)
// CHECK:       }
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions22OuterSinglePayloadNC_2OyxGlWOh"(
// CHECK-SAME:      ptr %0, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK:         call{{.*}} @"$s24moveonly_value_functions26InnerDeinitingReleasableNCVfD"(
// CHECK:       }
public func takeOuterSinglePayloadNC_2<T>(_ e: consuming OuterSinglePayloadNC_2<T>) {}

// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions26takeOuterSinglePayloadNC_3yyAA0efgH2_3OyxGnlF"(
// CHECK-SAME:      ptr noalias %0, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME: {
// CHECK-NOT:     @"$s24moveonly_value_functions28InnerDeinitingDestructableNCVMa"
// CHECK-NOT:     @"$s24moveonly_value_functions22OuterSinglePayloadNC_3OMa"
// CHECK:         call{{.*}} @"$s24moveonly_value_functions22OuterSinglePayloadNC_3OyxGlWOh"(
// CHECK-SAME:        ptr %0, 
// CHECK-SAME:        ptr %T)
// CHECK:       }
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions22OuterSinglePayloadNC_3OyxGlWOh"(
// CHECK-SAME:      ptr %0, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK:         [[RESPONSE:%[^,]+]] = call{{.*}} @"$s24moveonly_value_functions28InnerDeinitingDestructableNCVMa"(
//           :        i64 0, 
// CHECK-SAME:        ptr %T)
// CHECK:         [[METADATA:%[^,]+]] = extractvalue %swift.metadata_response [[RESPONSE:%[^,]+]], 0
// CHECK:         call{{.*}} @"$s24moveonly_value_functions28InnerDeinitingDestructableNCVfD"(
// CHECK-SAME:        ptr [[METADATA:%[^,]+]], 
//           :        ptr noalias swiftself %0)
// CHECK:       }
public func takeOuterSinglePayloadNC_3<T>(_ e: consuming OuterSinglePayloadNC_3<T>) {}

// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions25takeOuterMultiPayloadNC_1yyAA0efgH2_1OyxGnlF"(
// CHECK-SAME:      ptr noalias %0, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK-NOT:     @"$s24moveonly_value_functions26InnerDeinitingWithLayoutNCVMa"
// CHECK-NOT:     @"$s24moveonly_value_functions21OuterMultiPayloadNC_1OMa"
// CHECK:         call{{.*}} @"$s24moveonly_value_functions21OuterMultiPayloadNC_1OyxGlWOh"(
// CHECK-SAME:        ptr %0, 
// CHECK-SAME:        ptr %T)
// CHECK:       }
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions21OuterMultiPayloadNC_1OyxGlWOh"(
// CHECK-SAME:      ptr %0, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK:         [[RESPONSE1:%[^,]+]] = call{{.*}} @"$s24moveonly_value_functions26InnerDeinitingWithLayoutNCVMa"(
//           :        i64 0, 
// CHECK-SAME:        ptr %T)
// CHECK:         [[METADATA1:%[^,]+]] = extractvalue %swift.metadata_response [[RESPONSE1]], 0
// CHECK:         call{{.*}} @"$s24moveonly_value_functions26InnerDeinitingWithLayoutNCVfD"(
// CHECK-SAME:        ptr [[METADATA1]], 
// CHECK-SAME:        ptr noalias swiftself %0)
// CHECK:         [[RESPONSE2:%[^,]+]] = call{{.*}} @"$s24moveonly_value_functions26InnerDeinitingWithLayoutNCVMa"(
//           :        i64 0, 
// CHECK-SAME:        ptr %T)
// CHECK:         [[METADATA2:%[^,]+]] = extractvalue %swift.metadata_response [[RESPONSE2]], 0
// CHECK:         call{{.*}} @"$s24moveonly_value_functions26InnerDeinitingWithLayoutNCVfD"(
// CHECK-SAME:        ptr [[METADATA2]], 
// CHECK-SAME:        ptr noalias swiftself %0)
// CHECK:       }
public func takeOuterMultiPayloadNC_1<T>(_ e: consuming OuterMultiPayloadNC_1<T>) {}
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions25takeOuterMultiPayloadNC_2yyAA0efgH2_2OyxGnlF"(
//           :      i64 %0, 
//           :      i8 %1, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK:         call{{.*}} @"$s24moveonly_value_functions21OuterMultiPayloadNC_2OyxGlWOe"(
//           :        i64 %0, 
//           :        i8 %1, 
// CHECK-SAME:        ptr %T)
// CHECK:       }
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions21OuterMultiPayloadNC_2OyxGlWOe"(
//           :      i64 %0, 
//           :      i8 %1, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK:         call{{.*}} @"$s24moveonly_value_functions29InnerDeinitingWithoutLayoutNCVfD"(
//           :        i64 %0, 
// CHECK-SAME:        ptr %T)
// CHECK:         call{{.*}} @"$s24moveonly_value_functions29InnerDeinitingWithoutLayoutNCVfD"(
//           :        i64 %0, 
// CHECK-SAME:        ptr %T)
// CHECK:       }
public func takeOuterMultiPayloadNC_2<T>(_ e: consuming OuterMultiPayloadNC_2<T>) {}
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions25takeOuterMultiPayloadNC_3yyAA0efgH2_3OyxGnlF"(
//           :      ptr noalias {{(nocapture|captures\(none\))}} dereferenceable(64) %0, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK:         call{{.*}} @"$s24moveonly_value_functions21OuterMultiPayloadNC_3OyxGlWOh"(
// CHECK-SAME:        ptr %0, 
// CHECK-SAME:        ptr %T)
// CHECK:       }
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions21OuterMultiPayloadNC_3OyxGlWOh"(
// CHECK-SAME:      ptr %0, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK:         call{{.*}} @"$s24moveonly_value_functions21OuterMultiPayloadNC_3OyxGlWOe"(
// CHECK-SAME:        ptr %T)
// CHECK:       }
public func takeOuterMultiPayloadNC_3<T>(_ e: consuming OuterMultiPayloadNC_3<T>) {}
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions25takeOuterMultiPayloadNC_4yyAA0efgH2_4OyxGnlF"(
// CHECK-SAME:      ptr noalias %0, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK-NOT:     @"$s24moveonly_value_functions28InnerDeinitingDestructableNCVMa"
// CHECK-NOT:     @"$s24moveonly_value_functions21OuterMultiPayloadNC_4OMa"
// CHECK:         call{{.*}} @"$s24moveonly_value_functions21OuterMultiPayloadNC_4OyxGlWOh"(
// CHECK-SAME:        ptr %0, 
// CHECK-SAME:        ptr %T)
// CHECK:       }
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions21OuterMultiPayloadNC_4OyxGlWOh"(
// CHECK-SAME:      ptr %0, 
// CHECK-SAME:      ptr %T)
// CHECK-SAME:  {
// CHECK:         [[RESPONSE1:%[^,]+]] = call{{.*}} @"$s24moveonly_value_functions28InnerDeinitingDestructableNCVMa"(
//           :        i64 0, 
// CHECK-SAME:        ptr %T)
// CHECK:         [[METADATA1:%[^,]+]] = extractvalue %swift.metadata_response [[RESPONSE1]], 0
// CHECK:         call{{.*}} @"$s24moveonly_value_functions28InnerDeinitingDestructableNCVfD"(
// CHECK-SAME:        ptr [[METADATA1]], 
// CHECK-SAME:        ptr noalias swiftself %0)
// CHECK:         [[RESPONSE2:%[^,]+]] = call{{.*}} @"$s24moveonly_value_functions28InnerDeinitingDestructableNCVMa"(
//           :        i64 0, 
// CHECK-SAME:        ptr %T)
// CHECK:         [[METADATA2:%[^,]+]] = extractvalue %swift.metadata_response [[RESPONSE2]], 0
// CHECK:         call{{.*}} @"$s24moveonly_value_functions28InnerDeinitingDestructableNCVfD"(
// CHECK-SAME:        ptr [[METADATA2]], 
// CHECK-SAME:        ptr noalias swiftself %0)
// CHECK:       }
public func takeOuterMultiPayloadNC_4<T>(_ e: consuming OuterMultiPayloadNC_4<T>) {}

public struct EmptyDeinitingNC_1<Wrapped: ~Copyable>: ~Copyable {
  deinit {}
}

public enum SinglePayloadNC_1<Element: Equatable>: ~Copyable {
  case empty
  case node(EmptyDeinitingNC_1<Self>, Element)
}
// CHECK-LABEL: define{{.*}} @"$s24moveonly_value_functions18EmptyDeinitingNC_1VyAA013SinglePayloadF2_1OyxGG_xtSQRzlWOh"(
// CHECK-SAME:      ptr %0, 
// CHECK-SAME:      ptr %Element,
// CHECK-SAME:      ptr %Element.Equatable)
// CHECK-SAME:  {
// CHECK:         [[RESPONSE:%[^,]+]] = call swiftcc %swift.metadata_response @"$s24moveonly_value_functions17SinglePayloadNC_1OMa"(
//           :        i64 0, 
// CHECK-SAME:        ptr %Element,
// CHECK-SAME:        ptr %Element.Equatable)
// CHECK:         [[METADATA:%[^,]+]] = extractvalue %swift.metadata_response [[RESPONSE]], 0
// CHECK:         call swiftcc void @"$s24moveonly_value_functions18EmptyDeinitingNC_1VAARi_zrlEfD"(
// CHECK-SAME:        ptr [[METADATA]])
// CHECK:       }

