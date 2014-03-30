// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 %s

import Foundation

// Test if an instance method marked __attribute__((unavailable)) on
// the *class* NSObject can be used.
func test_unavailable_instance_method(x : NSObject) -> Bool {
  return x.allowsWeakReference() // expected-error {{'allowsWeakReference' is unavailable}}
}

func test_unavailable_func(x : NSObject) {
  NSDeallocateObject(x) // expected-error {{'NSDeallocateObject' is unavailable}}
}

