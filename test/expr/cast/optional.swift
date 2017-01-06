// RUN: %target-typecheck-verify-swift

class Base : Hashable { 
  var hashValue: Int { return 0 }
}

class Derived : Base { }

func ==(lhs: Base, rhs: Base) -> Bool { return false }

func ..<(x: Int?, y: Int) -> Int? { return x }

// Inputs that are more optional than the output.
func f1(i: Int?, ii: Int??, a: [Base]?, d: [Base : Base]?, de: Derived?) {
  _ = i as! Int // expected-warning{{forced cast from 'Int?' to 'Int' only unwraps optionals; did you mean to use '!'?}}{{8-8=!}}{{8-16=}}
  _ = ii as! Int // expected-warning{{forced cast from 'Int??' to 'Int' only unwraps optionals; did you mean to use '!!'?}}{{9-9=!!}}{{9-17=}}
  _ = a as! [Base] // expected-warning{{forced cast from '[Base]?' to '[Base]' only unwraps optionals; did you mean to use '!'?}}{{8-8=!}}{{8-19=}}
  _ = d as! [Base : Base] // expected-warning{{forced cast from '[Base : Base]?' to '[Base : Base]' only unwraps optionals; did you mean to use '!'?}}{{8-8=!}}{{8-26=}}
  _ = de as! Base // expected-warning{{forced cast from 'Derived?' to 'Base' only unwraps optionals; did you mean to use '!'?}}{{9-9=!}}{{9-18=}}

  // Conditional casts
  _ = i as? Int // expected-warning{{conditional downcast from 'Int?' to 'Int' does nothing}}{{8-16=}}
  _ = ii as? Int
  _ = a as? [Base] // expected-warning{{conditional downcast from '[Base]?' to '[Base]' does nothing}}{{8-19=}}
  _ = d as? [Base : Base] // expected-warning{{conditional downcast from '[Base : Base]?' to '[Base : Base]' does nothing}}{{8-26=}}
  _ = de as? Base // expected-warning{{conditional downcast from 'Derived?' to 'Base' is equivalent to an implicit conversion to an optional 'Base'}}{{9-18=}}

  // "is" checks
  _ = i is Int // expected-warning{{checking a value with optional type 'Int?' against dynamic type 'Int' succeeds whenever the value is non-'nil'; did you mean to use '!= nil'?}}{{9-15=!= nil}}
  _ = i..<1 is Int // expected-warning{{checking a value with optional type 'Int?' against dynamic type 'Int' succeeds whenever the value is non-'nil'; did you mean to use '!= nil'?}}{{7-7=(}}{{12-12=)}}{{13-19=!= nil}}
  _ = ii is Int

  _ = i..<1 is Bool // expected-warning{{cast from 'Int?' to unrelated type 'Bool' always fails}}
}

func implicitCastOfLiteralToOptional() {
  var _: Int? = 0
  var _: String? = ""
  var _: [Int] = []
  var _: [Int : Int]? = [:]
  var _: Set<Int> = []
}

