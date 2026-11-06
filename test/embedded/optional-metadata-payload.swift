// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -enable-experimental-feature NoncopyableCasting -parse-as-library -module-name main | %FileCheck %s

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_NoncopyableCasting

// A cast whose source is an Optional of a non-`Copyable` existential is a cast
// of the Optional itself, which the runtime unwraps. Embedded Optional metadata
// therefore records the payload's metadata after the kind
// (514 = MetadataKind::Optional).

protocol P: ~Copyable { }
struct S: P, ~Copyable { }

// CHECK: @"$e4main1P_pRi_s_XPSgMf" = {{.*}} <{ ptr @"$e4main1P_pRi_s_XPSgWV", i64 514, ptr {{.*}}@"$e4main1P_pRi_s_XPMf"
// CHECK-LABEL: define {{.*}} @"$e4main3isSySbAA1P_pRi_s_XPSgF"
// CHECK: call {{.*}} @swift_dynamicCastTest(ptr {{.*}}, ptr {{.*}}@"$e4main1P_pRi_s_XPSgMf"
@inline(never) func isS(_ o: borrowing (any P & ~Copyable)?) -> Bool {
  o is S
}

@main struct Main {
  static func main() {
    let o: (any P & ~Copyable)? = S()
    _ = isS(o)
  }
}
