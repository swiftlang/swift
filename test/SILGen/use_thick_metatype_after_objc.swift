// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -enable-implicit-dynamic -verify %s
// rdar://98418860

// REQUIRES: objc_interop

import Foundation

@propertyWrapper
struct ExamplePropertyWrapper {
    var wrappedValue: Bool = false
}

@objcMembers
class ExampleClass {
    @ExamplePropertyWrapper
    static var exampleProperty: Bool
}

class ExampleCallingClass {
    func exampleSegfaultingCall() {
        ExampleClass.exampleProperty.toggle()
    }
}
