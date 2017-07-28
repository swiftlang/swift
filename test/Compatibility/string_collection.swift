// RUN: %target-typecheck-verify-swift -swift-version 3

extension String : Collection { // expected-warning{{conformance of 'String' to protocol 'Collection' was already stated in the type's module 'Swift'}}
  subscript (i: Index) -> String { // expected-note{{subscript 'subscript' will not be used to satisfy the conformance to 'Collection'}}
    get { return self }
    set { }
  }

  var first: Character? { return nil } // expected-note{{var 'first' will not be used to satisfy the conformance to 'Collection'}}

}

func testStringOps(s: String) {
  _ = s.isEmpty
}
