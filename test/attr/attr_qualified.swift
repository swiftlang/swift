// RUN: %target-typecheck-verify-swift

class Swift {
  struct Nested {}
}

func badType() -> Swift.Int { 0 } // expected-error {{'Int' is not a member type of 'Swift'}}
func goodType() -> @qualified Swift.Int { 0 } // no-error
func uglyType() -> @qualified Swift.Array<Swift.Nested> { [] } // no-error

func badInit() -> Int { Swift.Int(bitPattern: 0) } // expected-error {{type 'Swift' has no member 'Int'}}
func goodInit() -> Int { @qualified Swift.Int(bitPattern: 0) } // no-error
func uglyInit() -> Int { (@qualified Swift.Int)(bitPattern: 0) } // no-error

func badRef() { Swift.print("hello") } // expected-error {{type 'Swift' has no member 'print'}}
func goodRef() { @qualified Swift.print("hello") } // no-error
func uglyRef() { (@qualified Swift.print)("hello") } // no-error
