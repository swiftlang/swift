// RUN: %target-typecheck-verify-swift -import-objc-header %S/Inputs/conditional_conformances_objc.h

// REQUIRES: objc_interop

protocol Foo {}
extension ObjC: Foo where ObjectType == ObjC2 {}
// expected-error@-1{{type 'ObjC<ObjectType>' cannot conditionally conform to protocol 'Foo' because the type uses the Objective-C generics model}}

protocol Bar {}
extension ObjC: Bar where ObjectType: Bar {}
// expected-error@-1{{type 'ObjC<ObjectType>' cannot conditionally conform to protocol 'Bar' because the type uses the Objective-C generics model}}

extension ObjC {
    struct Struct {
        enum Enum {}
    }
    class Class<T> {}
}

extension ObjC.Struct: Foo where ObjectType == ObjC2 {}
// expected-error@-1{{type 'ObjC<ObjectType>.Struct' cannot conditionally conform to protocol 'Foo' because the type uses the Objective-C generics model}}
extension ObjC.Struct: Bar where ObjectType: Bar {}
// expected-error@-1{{type 'ObjC<ObjectType>.Struct' cannot conditionally conform to protocol 'Bar' because the type uses the Objective-C generics model}}

extension ObjC.Struct.Enum: Foo where ObjectType == ObjC2 {}
// expected-error@-1{{type 'ObjC<ObjectType>.Struct.Enum' cannot conditionally conform to protocol 'Foo' because the type uses the Objective-C generics model}}
extension ObjC.Struct.Enum: Bar where ObjectType: Bar {}
// expected-error@-1{{type 'ObjC<ObjectType>.Struct.Enum' cannot conditionally conform to protocol 'Bar' because the type uses the Objective-C generics model}}

extension ObjC.Class: Foo where T == ObjC2 {}
// expected-error@-1{{type 'ObjC<ObjectType>.Class<T>' cannot conditionally conform to protocol 'Foo' because the type uses the Objective-C generics model}}
extension ObjC.Class: Bar where T: Bar {}
// expected-error@-1{{type 'ObjC<ObjectType>.Class<T>' cannot conditionally conform to protocol 'Bar' because the type uses the Objective-C generics model}}

@objc protocol AtObjCProtocol {}

class X<T> {}

extension X: ObjCProtocol where T: Foo {}
// expected-error@-1{{type 'X<T>' cannot conditionally conform to @objc protocol 'ObjCProtocol' because Objective-C does not support conditional conformances}}
extension X: AtObjCProtocol where T: Foo {}
// expected-error@-1{{type 'X<T>' cannot conditionally conform to @objc protocol 'AtObjCProtocol' because Objective-C does not support conditional conformances}}
