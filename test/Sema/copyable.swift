// RUN: %target-typecheck-verify-swift

protocol P: Copyable {} 
struct S: P {}

typealias PleaseLetMeDoIt = Copyable 
typealias WhatIfIQualify = Swift.Copyable 

class C: Copyable {} 

struct MOStruct: Copyable, ~Copyable {}
// expected-error@-1 {{struct 'MOStruct' required to be 'Copyable' but is marked with '~Copyable'}}


func whatever<T>(_ t: T) where T: Copyable {} 
func vatever<T: Copyable>(_ t: T) {} 
func buttever(_ t: any Copyable) {}
func zuttever(_ t: some Copyable) {}

enum RockNRoll<T: Copyable> { 
 case isNoisePollution(Copyable) 
 case isMusic(T)
}

enum namespace {
  typealias Copyable = Int

  func Copyable() -> Copyable { return 0 }
}

extension Copyable { // expected-error {{cannot extend protocol 'Copyable'}}
  func hello() {}
}
