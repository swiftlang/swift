// RUN: %swift %s -verify

//===----------------------------------------------------------------------===//
// Generic function declarations
//===----------------------------------------------------------------------===//

func f0<T>(x : Int, y : Int) { }
func f1<T : Any>(x : Int, y : Int) { }
func f2<T : protocol<Range,Any>>(x : Int, y : Int) { }
func f3<T : () -> ()>(x : Int, y : Int) { } // expected-error{{expected a type name or protocol composition restricting 'T'}}
func f4<T>(x : T, y : T) { }
