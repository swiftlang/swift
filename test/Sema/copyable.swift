// RUN: %target-typecheck-verify-swift

protocol P: Copyable {} // expected-error {{'Copyable' is unavailable}}
struct S: P {}

typealias PleaseLetMeDoIt = Copyable // expected-error {{'Copyable' is unavailable}}
typealias WhatIfIQualify = Swift.Copyable // expected-error {{'Copyable' is unavailable}}

class C: Copyable {} // expected-error {{'Copyable' is unavailable}}

@_moveOnly struct MOStruct: Copyable {}
// expected-error@-1 {{move-only struct 'MOStruct' cannot conform to 'Copyable'}}
// expected-error@-2 {{'Copyable' is unavailable}}


func whatever<T>(_ t: T) where T: Copyable {} // expected-error {{'Copyable' is unavailable}}
func vatever<T: Copyable>(_ t: T) {} // expected-error {{'Copyable' is unavailable}}
func buttever(_ t: any Copyable) {} // expected-error {{'Copyable' is unavailable}}
func zuttever(_ t: some Copyable) {} // expected-error 2{{'Copyable' is unavailable}}

enum RockNRoll<T: Copyable> { // expected-error {{'Copyable' is unavailable}}
 case isNoisePollution(Copyable) // expected-error {{'Copyable' is unavailable}}
 case isMusic(T)
}

enum namespace {
  typealias Copyable = Int

  func Copyable() -> Copyable { return 0 }
}
