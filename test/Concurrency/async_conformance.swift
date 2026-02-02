// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-sil -o /dev/null -verify -strict-concurrency=targeted %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-sil -o /dev/null -verify -strict-concurrency=complete %s

// REQUIRES: objc_interop
// REQUIRES: concurrency

import Foundation

// async objc requirement, sync witness
@objc protocol Tracker {
    func track(event: String) async // expected-note {{protocol requires function 'track(event:)' with type '(String) async -> ()'}}
}
class Dog: NSObject, Tracker { // expected-error {{type 'Dog' does not conform to protocol 'Tracker'}} expected-note {{add stubs for conformance}}
    func track(event: String) {} // expected-note {{candidate is not 'async', but '@objc' protocol requirement is}}
}

// sync objc requirement, async witness
@objc protocol Runner {
    func run(event: String) // expected-note {{protocol requires function 'run(event:)' with type '(String) -> ()'}}
}
class Athlete: NSObject, Runner { // expected-error {{type 'Athlete' does not conform to protocol 'Runner'}} expected-note {{add stubs for conformance}}
    func run(event: String) async {} // expected-note {{candidate is 'async', but '@objc' protocol requirement is not}}
}


// async swift protocol, sync witness
protocol Snacker {
    func snack(food: String) async
}

class Foodie: Snacker {
    func snack(food: String) {}
}


// sync swift protocol, async witness
protocol Backer {
    func back(stonk: String) // expected-note {{protocol requires function 'back(stonk:)' with type '(String) -> ()'}}
}

class Investor: Backer {  // expected-error {{type 'Investor' does not conform to protocol 'Backer'}} expected-note {{add stubs for conformance}}
    func back(stonk: String) async {}  // expected-note {{candidate is 'async', but protocol requirement is not}}
}
