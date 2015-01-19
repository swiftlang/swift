// RUN: %target-parse-verify-swift

extension Array {
  public func foo() { } // expected-error{{extension of generic type 'Array<T>' from a different module cannot provide public declarations}}

  public var bar: Int { return 5 }  // expected-error{{extension of generic type 'Array<T>' from a different module cannot provide public declarations}}

  public subscript (string: String) -> Int {  // expected-error{{extension of generic type 'Array<T>' from a different module cannot provide public declarations}}
    get { return 5 }
  }

  private func okay() { }
}

public extension Array {  // okay by itself
  private func wibble() { } // okay
  func wobble() { }  // expected-error{{extension of generic type 'Array<T>' from a different module cannot provide public declarations}}
}
