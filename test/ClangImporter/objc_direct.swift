// RUN: %target-swift-frontend-verify -typecheck %s -enable-objc-interop -import-objc-header %S/../Inputs/objc_direct.h

// REQUIRES: objc_interop

var something = Bar() as AnyObject

something.directProperty = 123      // expected-error {{value of type 'AnyObject' has no member 'directProperty'}}
let _ = something.directProperty    // expected-error {{value of type 'AnyObject' has no member 'directProperty'}}

something.directProperty2 = 456     // expected-error {{value of type 'AnyObject' has no member 'directProperty2'}}
let _ = something.directProperty2   // expected-error {{value of type 'AnyObject' has no member 'directProperty2'}}

let _ = something.directMethod()    // expected-error {{value of type 'AnyObject' has no member 'directMethod'}}

let _ = something.directMethod2()   // expected-error {{value of type 'AnyObject' has no member 'directMethod2'}}

class Foo {
    // Test that we can use a method with the same name as some `objc_direct` method.
    @objc func directProtocolMethod() -> String {
        "This is not a direct method."
    }
}

var otherthing = Foo() as AnyObject

// We expect no error.
let _ = otherthing.directProtocolMethod()
