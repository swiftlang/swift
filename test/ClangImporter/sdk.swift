// RUN: %target-swift-frontend -typecheck -verify %s

// XFAIL: linux

import Foundation

// Swift and Foundation types should work.

func available_Int(_ a: Int) {}
func available_DateFormatter(_ a: DateFormatter) {}


// Some traditional Objective-C types should fail with fixits.

func unavailable_id(_ a: id) {} // expected-error {{'id' is unavailable in Swift: 'id' is not available in Swift; use 'Any'}}
func unavailable_Class(_ a: Class) {} // expected-error {{use of undeclared type 'Class'; did you mean to use 'AnyClass'?}} {{29-34=AnyClass}}
func unavailable_BOOL(_ a: BOOL) {} // expected-error {{use of undeclared type 'BOOL'; did you mean to use 'ObjCBool'?}} {{28-32=ObjCBool}}
func unavailable_SEL(_ a: SEL) {} // expected-error {{use of undeclared type 'SEL'; did you mean to use 'Selector'?}} {{27-30=Selector}}
func unavailable_NSUInteger(_ a: NSUInteger) {} // expected-error {{use of undeclared type 'NSUInteger'; did you mean to use 'Int'?}} {{34-44=Int}} expected-note {{did you mean to use 'UInt'?}} {{34-44=UInt}}
