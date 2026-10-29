// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-as-library -enable-experimental-feature BackDeployFrozenValueTypes -enable-library-evolution -module-name MyKit %S/../IRGen/Inputs/back_deployed_type_module.swift -o %t/MyKit.swiftmodule
// RUN: %target-swift-frontend -emit-ir -o /dev/null -parse-as-library -module-name Client -package-name Client -validate-tbd-against-ir=all %s -I %t
// RUN: %target-swift-frontend -emit-sil -parse-as-library -module-name Client -package-name Client %s -I %t | %FileCheck %s

// REQUIRES: swift_feature_BackDeployFrozenValueTypes
// REQUIRES: OS=macosx
// REQUIRES: CPU=arm64 || CPU=x86_64

// A client that emits its own copy of a back-deployed type from another
// module still defines the members that it adds to the type in extensions, so
// they belong in the client's TBD. Their availability for linkage doesn't come
// from the version that the type is back-deployed before: this module
// introduces them.

import MyKit

extension Point {
  public func extraPublic() -> Int { x }
  package func extraPackage() -> Int { x }
}

// CHECK-NOT: [available {{.*}}@$s5MyKit5PointV6ClientE
// CHECK: sil @$s5MyKit5PointV6ClientE11extraPublicSiyF
// CHECK: sil package @$s5MyKit5PointV6ClientE12extraPackageSiyF
