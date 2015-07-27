// RUN: %target-swift-frontend %s -parse -verify

class A : A { } // expected-error {{circular class inheritance A}}

func doIt<T>(obj: AnyObject) -> T? {
  return obj as? T
}

let result: A? = doIt(A()) as A? // expected-error {{'A' cannot be constructed because it has no accessible initializers}}
