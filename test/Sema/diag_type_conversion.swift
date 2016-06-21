// RUN: %target-parse-verify-swift

func foo1(_ a: [Int]) {}
func foo2(_ a : UnsafePointer<Int>) {}
func foo4(_ a : UnsafeMutablePointer<Int>) {}
func foo3 () {
  foo1(1) // expected-error {{cannot convert value of type 'Int' to expected argument type '[Int]'}} {{8-8=[}} {{9-9=]}}
  foo2(1) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafePointer<Int>'}} {{8-8=&}}
  var j = 3
  foo2(j) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafePointer<Int>'}} {{8-8=&}}
  foo4(j) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafeMutablePointer<Int>'}} {{8-8=&}}
}
