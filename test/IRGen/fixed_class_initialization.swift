// RUN: %target-swift-frontend -target x86_64-apple-macosx10.15 -module-name main -emit-ir %s | %FileCheck --check-prefix=CHECK --check-prefix=HAS_OPT_SELF %s
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.14 -module-name main -emit-ir %s | %FileCheck --check-prefix=CHECK --check-prefix=NO_OPT_SELF %s

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

class C {
  var x: Int = 0
}

public func foof() -> Any.Type {
  return C.self
}

// CHECK-LABEL: define {{.*}} %swift.metadata_response @"$s4main1CCMa"
// HAS_OPT_SELF:  call {{.*}} @objc_opt_self
// NO_OPT_SELF:  call {{.*}} @swift_getInitializedObjCClass
