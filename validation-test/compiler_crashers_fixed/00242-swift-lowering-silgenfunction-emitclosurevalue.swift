// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -parse -verify

// Issue found by https://github.com/fluidsonic (Marc Knaup)

class A {
    class func a() -> String {
    }
    class func b() { // expected-note {{'self' declared here}}
        struct c { // expected-note {{type declared here}}
            static let d: String = {
                return self.a() // expected-error {{struct declaration cannot close over value 'self' defined in outer scope}}
            }()
        }
    }
}
