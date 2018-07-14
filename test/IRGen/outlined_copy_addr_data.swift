// RUN: %target-swift-frontend -emit-ir  -module-name outcopyaddr -primary-file %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

struct Resource<A> {
    let requestBody: Data?
    let parse: (Data) -> A?
}

// CHECK-LABEL: define hidden swiftcc void @"$S11outcopyaddr13CrashMetaTypeC10apiRequest4base8resourceySS_AA8ResourceVyxGtlFZ"
// CHECK: entry:
// CHECK: alloca
// CHECK: alloca
// CHECK: alloca
// CHECK: store
// CHECK: store
// CHECK: store
// CHECK: ret void
class CrashMetaType {
    required init() { }
    class func apiRequest<A>(base: String, resource: Resource<A>) {}
}
