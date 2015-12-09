// RUN: %target-swift-frontend -disable-implicit-optional-conversion -emit-sil %s

func f(x : Int?) -> Int {
  if let y = x {
    return y
  }
  return 0
}

let x : Int = 5

f(x) // expected-error {{cannot convert value of type 'Int' to expected argument type 'Int?'}}

let y : Int? = 5 // expected-error {{cannot convert value of type 'Int' to specified type 'Int?'}}

f(y)

let z : Int?! = .Some(.Some(5)) // expected-error {{cannot convert value of type 'Int?!' to expected argument type 'Int?'}}

f(z)


