// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 %s

import Foundation
import stdio

// Test if an instance method marked __attribute__((unavailable)) on
// the *class* NSObject can be used.
func test_unavailable_instance_method(x : NSObject) -> Bool {
  return x.allowsWeakReference() // expected-error {{'allowsWeakReference' is unavailable}}
}

func test_unavailable_method_in_protocol(x : NSObjectProtocol) {
  x.retain() // expected-error {{'retain' is unavailable}}
}
func test_unavailable_method_in_protocol_use_class_instance(x : NSObject) {
  x.retain() // expected-error {{'retain' is unavailable}}
}

func test_unavailable_func(x : NSObject) {
  NSDeallocateObject(x) // expected-error {{'NSDeallocateObject' is unavailable}}
}

func test_deprecated_imported_as_unavailable(s:CMutablePointer<CChar>) {
  let x = tmpnam(s) // expected-error {{'tmpnam' is unavailable: Due to security concerns inherent in the design of tmpnam(3), it is highly recommended that you use mkstemp(3) instead.}}
}

func test_NSInvocation(x:NSInvocation) {} // expected-error {{'NSInvocation' is unavailable}}

func test_class_avail(x:NSObject, obj: AnyObject) {
  x.`class`() // expected-error {{'class' is unavailable: use 'dynamicType' instead}}
  NSObject.`class`() // expected-error {{'class' is unavailable: use 'self' instead}}
  
}
