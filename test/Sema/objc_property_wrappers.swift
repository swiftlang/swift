// RUN: %target-swift-frontend -typecheck -verify -enable-objc-interop %s

// REQUIRES: objc_interop

import Foundation

class ObjcClassCase {
    @objc
    func foo(@WrapperObjcClass _ ref: Int) throws {}
}

@propertyWrapper @objc
public class WrapperObjcClass: NSObject {
    public var wrappedValue: Int

    public init(wrappedValue: Int) {
        self.wrappedValue = wrappedValue
    }
    
    public init(projectedValue: WrapperObjcClass) {
        fatalError()
    }

    public var projectedValue: WrapperObjcClass {
        fatalError()
    }
}

class GenericClassCase {
    @objc
    func foo(@WrapperGenericClass _ ref: Int) throws {} // expected-error {{method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
    // expected-note@-1 {{classes not annotated with '@objc' cannot be represented in Objective-C}}
}

@propertyWrapper
public class WrapperGenericClass<Element> {
    public var wrappedValue: Element

    public init(wrappedValue: Element) {
        self.wrappedValue = wrappedValue
    }
    
    public init(projectedValue: WrapperGenericClass<Element>) {
        fatalError()
    }

    public var projectedValue: WrapperGenericClass<Element> {
        fatalError()
    }
}

class StructCase {
    @objc
    func foo(@WrapperStruct _ ref: Int) throws {} // expected-error {{method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
    // expected-note@-1 {{Swift structs cannot be represented in Objective-C}}
}

@propertyWrapper
public struct WrapperStruct {
    public var wrappedValue: Int

    public init(wrappedValue: Int) {
        self.wrappedValue = wrappedValue
    }
    
    public init(projectedValue: WrapperStruct) {
        fatalError()
    }

    public var projectedValue: WrapperStruct {
        fatalError()
    }
}

class EnumCase {
    @objc
    func foo(@WrapperEnum _ ref: Int) throws {} // expected-error {{method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
    // expected-note@-1 {{non-'@objc' enums cannot be represented in Objective-C}}
}

@propertyWrapper
public enum WrapperEnum {
    public var wrappedValue: Int {
    	fatalError()
    }

    public init(wrappedValue: Int) {
    }
    
    public init(projectedValue: WrapperStruct) {
        fatalError()
    }

    public var projectedValue: WrapperStruct {
        fatalError()
    }
}
