// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -verify

// REQUIRES: objc_interop

import Foundation

class A: NSObjectProtocol { } // expected-error{{cannot declare conformance to 'NSObjectProtocol' in Swift; 'A' should inherit 'NSObject' instead}}{{10-26=NSObject}}

@objc protocol Other: NSObjectProtocol { }

class B: Other { } // expected-error{{cannot declare conformance to 'NSObjectProtocol' in Swift; 'B' should inherit 'NSObject' instead}}

class C { }

class D: C, NSObjectProtocol { } // expected-error{{cannot declare conformance to 'NSObjectProtocol' in Swift; 'D' should inherit 'NSObject' instead}}

class E { }

extension E: NSObjectProtocol { } // expected-error{{cannot declare conformance to 'NSObjectProtocol' in Swift; 'E' should inherit 'NSObject' instead}}
