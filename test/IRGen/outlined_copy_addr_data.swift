// RUN: %target-swift-frontend -emit-ir  -module-name outcopyaddr -primary-file %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

struct Resource<A> {
    let requestBody: Data?
    let parse: (Data) -> A?
}

// CHECK-LABEL: define hidden swiftcc void @"$S11outcopyaddr13CrashMetaTypeC10apiRequest4base8resourceySS_AA8ResourceVyxGtlFZ"
// CHECK: call %T11outcopyaddr8ResourceV* @"$S11outcopyaddrytWh2_"(%T11outcopyaddr8ResourceV*
// CHECK: ret void
class CrashMetaType {
    required init() { }
    class func apiRequest<A>(base: String, resource: Resource<A>) {}
}
