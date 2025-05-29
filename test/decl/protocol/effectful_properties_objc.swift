// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

// REQUIRES: objc_interop

/////////
// check for disallowed attributes in protocols
@objc protocol Tea {
 var temperature : Double { get throws } // expected-error{{property with 'throws' or 'async' is not representable in Objective-C}} expected-note{{inferring '@objc' because the declaration is a member of an '@objc' protocol}}
 subscript(_ d : Double) -> Bool { get async throws } // expected-error{{subscript with 'throws' or 'async' is not representable in Objective-C}} expected-note{{inferring '@objc' because the declaration is a member of an '@objc' protocol}}

 // NOTE: this seems counter-intuitive, but TSPL says @nonobjc applies to
 // members that are representable in ObjC, and this is not representable.
 @nonobjc var sugar : Bool { get async } // expected-error{{property with 'throws' or 'async' is not representable in Objective-C}} expected-note{{inferring '@objc' because the declaration is a member of an '@objc' protocol}}
}
