// RUN: %target-typecheck-verify-swift -parse-as-library -swift-version 3

class A { 
  @objc func objcVirtualFunction() { } // expected-note{{overridden declaration is here}}
}

class B : A { }

extension B { 
  override func objcVirtualFunction() { } // expected-warning{{cannot override a non-dynamic class declaration from an extension}}
}
