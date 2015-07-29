// RUN: %target-swift-frontend %s -parse -verify

let pq = {
    return $0 ?? nil // expected-error{{argument for generic parameter 'T' could not be inferred}}
}
