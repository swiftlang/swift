// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -typecheck -verify

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
