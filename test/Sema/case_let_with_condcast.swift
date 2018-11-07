// RUN: %target-typecheck-verify-swift 

// from bug SR-2022

let a: Any
let b: Any

switch a {

case let arr as [Int]:
    print(arr)

    // expected-error@+1 {{'as' in 'case' statement dose not use '?'}}
case let n as? Float: 
    print(n)

    // expected-error@+1 {{'as' in 'case' statement dose not use '!'}}
case let s as! String: 
    print(s)

    // expected-error@+2 {{'as' in 'case' statement dose not use '?'}}	
case let n as Int where n == 0,
     let n as? Int where n == 1:
    print(n)

    // We should still be allowed to use as? in the guard expression etc.	
case let n as Int where n == (b as? Int):
    print("It's the same as `b`, \(b as? Int)")

default: print("Some other value...")
} 
