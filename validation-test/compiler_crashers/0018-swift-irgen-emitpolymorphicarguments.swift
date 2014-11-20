// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/owensd (David Owens II)

func a<T>() {
    enum b { // expected-error {{type 'b' nested in generic function 'a' is not allowed}}
        case c
    }
}
