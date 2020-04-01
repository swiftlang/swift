// RUN: %target-swift-frontend -target x86_64-apple-macos10.15 -stack-check -emit-ir %s 2>&1 | %FileCheck %s
// REQUIRES: objc_interop

public func testFunc() {
}

// CHECK: define{{.*}} swiftcc void @"$s4main8testFuncyyF"() #[[ATTRNR:[0-9]+]]
// CHECK: attributes #[[ATTRNR]] = { {{.*}}"probe-stack"="__chkstk_darwin"
