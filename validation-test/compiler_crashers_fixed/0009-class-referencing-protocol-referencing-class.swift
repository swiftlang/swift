// RUN: %target-swift-frontend %s -verify -emit-ir

// Test case submitted to project by https://github.com/practicalswift (practicalswift)

protocol b {
    var a: c<b> { // expected-error 2{{type 'b' does not conform to protocol 'b'}}
        get
    }
}

class c<d : b> {
}
