// RUN: %target-swift-frontend -typecheck -verify -enable-objc-interop %s
// RUN: %target-swift-frontend -typecheck -verify -enable-objc-interop -parse-as-library %s

// REQUIRES: objc_interop

import Foundation

class NonObjCBase {}
protocol NonObjCProto {}

@objc class AddSuper {} // expected-error {{only classes that inherit from NSObject can be declared '@objc'}} {{1-7=}}
// expected-note@-1 {{inherit from 'NSObject' to silence this error}} {{21-21=: NSObject}}

@objc class InsertSuper: NonObjCProto {} // expected-error {{only classes that inherit from NSObject can be declared '@objc'}} {{1-7=}}
// expected-note@-1 {{inherit from 'NSObject' to silence this error}} {{26-26=NSObject, }}

@objc class NoNote: NonObjCBase {} // expected-error {{only classes that inherit from NSObject can be declared '@objc'}} {{1-7=}}
