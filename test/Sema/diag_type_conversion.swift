// RUN: %target-parse-verify-swift

func foo1(_ a: [Int]) {}
func foo2(_ a : UnsafePointer<Int>) {}
func foo4(_ a : UnsafeMutablePointer<Int>) {}
func foo3 () {
  let j = 3
  foo2(j) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafePointer<Int>'}} {{9-9= as! UnsafePointer<Int>}}
  foo4(j) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafeMutablePointer<Int>'}} {{9-9= as! UnsafeMutablePointer<Int>}}

  var i = 3
  foo2(i) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafePointer<Int>'}} {{8-8=&}}
  foo4(i) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafeMutablePointer<Int>'}} {{8-8=&}}

  foo2(1) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafePointer<Int>'}} {{9-9= as! UnsafePointer<Int>}}
  foo4(1) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafeMutablePointer<Int>'}} {{9-9= as! UnsafeMutablePointer<Int>}}
}
