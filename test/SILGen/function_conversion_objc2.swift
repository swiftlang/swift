// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name function_conversion_objc -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -enable-objc-interop -verify

// We can't put these in function_conversion_objc.swift, because the error that's
// picked up below happens before we get to SILGen, so we then don't output the
// SIL.  Hence this has been moved to its own separate file.

import Foundation

func cFuncPtrConversionUnsupported(_ x: @escaping @convention(c) (@convention(block) () -> ()) -> ())
    -> @convention(c) (@convention(c) () -> ()) -> () {
  return x  // expected-error{{cannot convert return expression of type '@convention(c) (@convention(block) () -> ()) -> ()' to return type '@convention(c) (@convention(c) () -> ()) -> ()'}}
}

class A : NSObject {}
class B : A {}

// Can't convert a function taking a B to a function taking an A, because it
// expects a B, not an A.
func cFuncPtrConversionWrong(_ x: @escaping @convention(c) (B) -> ()) -> @convention(c) (A) -> () {
  return x  // expected-error{{cannot convert return expression of type '@convention(c) (B) -> ()' to return type '@convention(c) (A) -> ()'}}
}

// Can't convert a function returning an A to a function returning a B, because
// not all 'A's are 'B's
func cFuncPtrConversionWrong2(_ x: @escaping @convention(c) () -> A) -> @convention(c) () -> B {
  return x  // expected-error{{cannot convert return expression of type '@convention(c) () -> A' to return type '@convention(c) () -> B'}}
}
