// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

@objc protocol AProtocol {
    @objc optional func dynamicMethod()
    // CHECK: [[@LINE-1]]:25 | instance-method/Swift | dynamicMethod() | [[DynamicMethod_USR:.*]] | Def
    @objc optional var property: String { get }
    // CHECK: [[@LINE-1]]:24 | instance-property/Swift | property | [[DynamicProperty_USR:.*]] | Def
}

class AClass {

    weak var objcDelegate: AProtocol?
    // CHECK: [[@LINE-1]]:14 | instance-property/Swift | objcDelegate | [[Delegate_USR:.*]] | Def

    func doSomething() {
        objcDelegate?.dynamicMethod?()
        // CHECK: [[@LINE-1]]:9 | instance-property/Swift | objcDelegate | [[Delegate_USR]] | Ref
        // CHECK: [[@LINE-2]]:23 | instance-method/Swift | dynamicMethod() | [[DynamicMethod_USR]] | Ref
        _ = objcDelegate?.property
        // CHECK: [[@LINE-1]]:13 | instance-property/Swift | objcDelegate | [[Delegate_USR]] | Ref
        // CHECK: [[@LINE-2]]:27 | instance-property/Swift | property | [[DynamicProperty_USR]] | Ref
    }
}
