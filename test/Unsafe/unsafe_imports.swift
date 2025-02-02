// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/unsafe_swift_decls.swiftmodule %S/Inputs/unsafe_swift_decls.swift -enable-experimental-feature AllowUnsafeAttribute

// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -I %S/Inputs -I %t

// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_WarnUnsafe

import unsafe_decls
import unsafe_swift_decls

// expected-warning@+3{{global function 'testUnsafe' has an interface that involves unsafe types}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{1-1=@safe }}
func testUnsafe(_ ut: UnsafeType) { // expected-note{{reference to unsafe struct 'UnsafeType'}}
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{3-3=unsafe }}
  unsafe_c_function() // expected-note{{reference to unsafe global function 'unsafe_c_function()'}}

  var array: [CInt] = [1, 2, 3, 4, 5]
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{3-3=unsafe }}
  print_ints(&array, CInt(array.count))
  // expected-note@-1{{reference to global function 'print_ints' involves unsafe type 'UnsafeMutablePointer<Int32>'}}
}

// Reference a typealias that isn't itself @unsafe, but refers to an unsafe
// type.

// expected-warning@+3{{global function 'testUnsafeThroughAlias' has an interface that involves unsafe types}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{1-1=@safe }}
func testUnsafeThroughAlias(_ ut: UnsafeTypeAlias) { // expected-note{{reference to type alias 'UnsafeTypeAlias' involves unsafe type 'UnsafeTypeAlias' (aka 'PointerType')}}
  // TODO: Diagnostic above could be better
}


struct ConformsToUnsafeRequirement: HasUnsafeRequirement {
  @unsafe func f(_: PointerType) { }
}

class SubclassWithUnsafeMethod: SuperclassWithUnsafeMethod {
  @unsafe override func implicitlyUnsafe(_: PointerType) { }
}
