// RUN: %target-swift-frontend %s -parse -verify

// Issue found by https://github.com/owensd (David Owens II)

func a<T>() { // expected-error {{generic parameter 'T' is not used in function signature}}
    enum b { // expected-error {{type 'b' nested in generic function 'a' is not allowed}}
        case c
    }
}
