// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/unsafe_swift_decls.swiftmodule %S/Inputs/unsafe_swift_decls.swift

// RUN: %target-typecheck-verify-swift -strict-memory-safety -I %S/Inputs -I %t

import unsafe_decls
import unsafe_swift_decls

func testUnsafe(_ ut: UnsafeType) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{3-3=unsafe }}
  unsafe_c_function() // expected-note{{reference to unsafe global function 'unsafe_c_function()'}}

  var array: [CInt] = [1, 2, 3, 4, 5]
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{3-3=unsafe }}
  print_ints(&array, CInt(array.count))
  // expected-note@-1{{argument #0 in call to global function 'print_ints' has unsafe type 'UnsafeMutablePointer<Int32>?'}}

  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{7-7=unsafe }}
  _ = print_ints // expected-note{{reference to global function 'print_ints' involves unsafe type 'UnsafeMutablePointer<Int32>'}}
}

// Reference a typealias that isn't itself @unsafe, but refers to an unsafe
// type.

func testUnsafeThroughAlias(_ ut: UnsafeTypeAlias) {

}

func callThroughAlias(ut: UnsafeTypeAlias) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  testUnsafeThroughAlias(ut) // expected-note{{argument #0 in call to global function 'testUnsafeThroughAlias' has unsafe type 'UnsafeTypeAlias' (aka 'PointerType')}}
  // expected-note@-1{{reference to parameter 'ut' involves unsafe type 'UnsafeTypeAlias' (aka 'PointerType')}}
}


struct ConformsToUnsafeRequirement: HasUnsafeRequirement {
  @unsafe func f(_: PointerType) { }
}

class SubclassWithUnsafeMethod: SuperclassWithUnsafeMethod {
  @unsafe override func implicitlyUnsafe(_: PointerType) { }
}
