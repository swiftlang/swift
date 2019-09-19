// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// A top-level CodingKeys type to fall back to in lookups below.
public enum CodingKeys : String, CodingKey {
  case topLevel
}

// MARK: - Synthesized CodingKeys Enum

// Structs which get synthesized Codable implementations should have visible
// CodingKey enums during member type lookup.
struct SynthesizedStruct : Codable {
  let value: String = "foo"

  // Qualified type lookup should always be unambiguous.
  public func qualifiedFoo(_ key: SynthesizedStruct.CodingKeys) {} // expected-error {{method cannot be declared public because its parameter uses a private type}}
  internal func qualifiedBar(_ key: SynthesizedStruct.CodingKeys) {} // expected-error {{method cannot be declared internal because its parameter uses a private type}}
  fileprivate func qualfiedBaz(_ key: SynthesizedStruct.CodingKeys) {} // expected-error {{method cannot be declared fileprivate because its parameter uses a private type}}
  private func qualifiedQux(_ key: SynthesizedStruct.CodingKeys) {}

  // Unqualified lookups should find the synthesized CodingKeys type instead
  // of the top-level type above.
  public func unqualifiedFoo(_ key: CodingKeys) { // expected-error {{method cannot be declared public because its parameter uses a private type}}
    print(CodingKeys.value) // Not found on top-level.
  }

  internal func unqualifiedBar(_ key: CodingKeys) { // expected-error {{method cannot be declared internal because its parameter uses a private type}}
    print(CodingKeys.value) // Not found on top-level.
  }

  fileprivate func unqualifiedBaz(_ key: CodingKeys) { // expected-error {{method cannot be declared fileprivate because its parameter uses a private type}}
    print(CodingKeys.value) // Not found on top-level.
  }

  private func unqualifiedQux(_ key: CodingKeys) {
    print(CodingKeys.value) // Not found on top-level.
  }

  // Unqualified lookups should find the most local CodingKeys type available.
  public func nestedUnqualifiedFoo(_ key: CodingKeys) { // expected-error {{method cannot be declared public because its parameter uses a private type}}
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func foo(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    foo(CodingKeys.nested)
  }

  internal func nestedUnqualifiedBar(_ key: CodingKeys) { // expected-error {{method cannot be declared internal because its parameter uses a private type}}
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func bar(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    bar(CodingKeys.nested)
  }

  fileprivate func nestedUnqualifiedBaz(_ key: CodingKeys) { // expected-error {{method cannot be declared fileprivate because its parameter uses a private type}}
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func baz(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    baz(CodingKeys.nested)
  }

  private func nestedUnqualifiedQux(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func qux(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    qux(CodingKeys.nested)
  }

  // Lookup within nested types should look outside of the type.
  struct Nested {
    // Qualified lookup should remain as-is.
    public func qualifiedFoo(_ key: SynthesizedStruct.CodingKeys) {} // expected-error {{method cannot be declared public because its parameter uses a private type}}
    internal func qualifiedBar(_ key: SynthesizedStruct.CodingKeys) {} // expected-error {{method cannot be declared internal because its parameter uses a private type}}
    fileprivate func qualfiedBaz(_ key: SynthesizedStruct.CodingKeys) {} // expected-error {{method cannot be declared fileprivate because its parameter uses a private type}}
    private func qualifiedQux(_ key: SynthesizedStruct.CodingKeys) {}

    // Unqualified lookups should find the SynthesizedStruct's synthesized
    // CodingKeys type instead of the top-level type above.
    public func unqualifiedFoo(_ key: CodingKeys) { // expected-error {{method cannot be declared public because its parameter uses a private type}}
      print(CodingKeys.value) // Not found on top-level.
    }

    internal func unqualifiedBar(_ key: CodingKeys) { // expected-error {{method cannot be declared internal because its parameter uses a private type}}
      print(CodingKeys.value) // Not found on top-level.
    }

    fileprivate func unqualifiedBaz(_ key: CodingKeys) { // expected-error {{method cannot be declared fileprivate because its parameter uses a private type}}
      print(CodingKeys.value) // Not found on top-level.
    }

    private func unqualifiedQux(_ key: CodingKeys) {
      print(CodingKeys.value) // Not found on top-level.
    }

    // Unqualified lookups should find the most local CodingKeys type available.
    public func nestedUnqualifiedFoo(_ key: CodingKeys) { // expected-error {{method cannot be declared public because its parameter uses a private type}}
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func foo(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      foo(CodingKeys.nested)
    }

    internal func nestedUnqualifiedBar(_ key: CodingKeys) { // expected-error {{method cannot be declared internal because its parameter uses a private type}}
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func bar(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      bar(CodingKeys.nested)
    }

    fileprivate func nestedUnqualifiedBaz(_ key: CodingKeys) { // expected-error {{method cannot be declared fileprivate because its parameter uses a private type}}
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func baz(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      baz(CodingKeys.nested)
    }

    private func nestedUnqualifiedQux(_ key: CodingKeys) {
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func qux(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      qux(CodingKeys.nested)
    }
  }
}

// MARK: - No CodingKeys Enum

// Structs which don't get synthesized Codable implementations should expose the
// appropriate CodingKeys type.
struct NonSynthesizedStruct : Codable { // expected-note 4 {{'NonSynthesizedStruct' declared here}}
  // No synthesized type since we implemented both methods.
  init(from decoder: Decoder) throws {}
  func encode(to encoder: Encoder) throws {}

  // Qualified type lookup should clearly fail -- we shouldn't get a synthesized
  // type here.
  public func qualifiedFoo(_ key: NonSynthesizedStruct.CodingKeys) {} // expected-error {{'CodingKeys' is not a member type of 'NonSynthesizedStruct'}}
  internal func qualifiedBar(_ key: NonSynthesizedStruct.CodingKeys) {} // expected-error {{'CodingKeys' is not a member type of 'NonSynthesizedStruct'}}
  fileprivate func qualfiedBaz(_ key: NonSynthesizedStruct.CodingKeys) {} // expected-error {{'CodingKeys' is not a member type of 'NonSynthesizedStruct'}}
  private func qualifiedQux(_ key: NonSynthesizedStruct.CodingKeys) {} // expected-error {{'CodingKeys' is not a member type of 'NonSynthesizedStruct'}}

  // Unqualified lookups should find the public top-level CodingKeys type.
  public func unqualifiedFoo(_ key: CodingKeys) { print(CodingKeys.topLevel) }
  internal func unqualifiedBar(_ key: CodingKeys) { print(CodingKeys.topLevel) }
  fileprivate func unqualifiedBaz(_ key: CodingKeys) { print(CodingKeys.topLevel) }
  private func unqualifiedQux(_ key: CodingKeys) { print(CodingKeys.topLevel) }

  // Unqualified lookups should find the most local CodingKeys type available.
  public func nestedUnqualifiedFoo(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func foo(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on top-level type.
    }

    foo(CodingKeys.nested)
  }

  internal func nestedUnqualifiedBar(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func bar(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on top-level type.
    }

    bar(CodingKeys.nested)
  }

  fileprivate func nestedUnqualifiedBaz(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func baz(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on top-level type.
    }

    baz(CodingKeys.nested)
  }

  private func nestedUnqualifiedQux(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func qux(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on top-level type.
    }

    qux(CodingKeys.nested)
  }
}

// MARK: - Explicit CodingKeys Enum

// Structs which explicitly define their own CodingKeys types should have
// visible CodingKey enums during member type lookup.
struct ExplicitStruct : Codable {
  let value: String = "foo"

  public enum CodingKeys {
    case a
    case b
    case c
  }

  init(from decoder: Decoder) throws {}
  func encode(to encoder: Encoder) throws {}

  // Qualified type lookup should always be unambiguous.
  public func qualifiedFoo(_ key: ExplicitStruct.CodingKeys) {}
  internal func qualifiedBar(_ key: ExplicitStruct.CodingKeys) {}
  fileprivate func qualfiedBaz(_ key: ExplicitStruct.CodingKeys) {}
  private func qualifiedQux(_ key: ExplicitStruct.CodingKeys) {}

  // Unqualified lookups should find the synthesized CodingKeys type instead
  // of the top-level type above.
  public func unqualifiedFoo(_ key: CodingKeys) {
    print(CodingKeys.a) // Not found on top-level.
  }

  internal func unqualifiedBar(_ key: CodingKeys) {
    print(CodingKeys.a) // Not found on top-level.
  }

  fileprivate func unqualifiedBaz(_ key: CodingKeys) {
    print(CodingKeys.a) // Not found on top-level.
  }

  private func unqualifiedQux(_ key: CodingKeys) {
    print(CodingKeys.a) // Not found on top-level.
  }

  // Unqualified lookups should find the most local CodingKeys type available.
  public func nestedUnqualifiedFoo(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func foo(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    foo(CodingKeys.nested)
  }

  internal func nestedUnqualifiedBar(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func bar(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    bar(CodingKeys.nested)
  }

  fileprivate func nestedUnqualifiedBaz(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func baz(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    baz(CodingKeys.nested)
  }

  private func nestedUnqualifiedQux(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func qux(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    qux(CodingKeys.nested)
  }

  // Lookup within nested types should look outside of the type.
  struct Nested {
    // Qualified lookup should remain as-is.
    public func qualifiedFoo(_ key: ExplicitStruct.CodingKeys) {}
    internal func qualifiedBar(_ key: ExplicitStruct.CodingKeys) {}
    fileprivate func qualfiedBaz(_ key: ExplicitStruct.CodingKeys) {}
    private func qualifiedQux(_ key: ExplicitStruct.CodingKeys) {}

    // Unqualified lookups should find the ExplicitStruct's synthesized
    // CodingKeys type instead of the top-level type above.
    public func unqualifiedFoo(_ key: CodingKeys) {
      print(CodingKeys.a) // Not found on top-level.
    }

    internal func unqualifiedBar(_ key: CodingKeys) {
      print(CodingKeys.a) // Not found on top-level.
    }

    fileprivate func unqualifiedBaz(_ key: CodingKeys) {
      print(CodingKeys.a) // Not found on top-level.
    }

    private func unqualifiedQux(_ key: CodingKeys) {
      print(CodingKeys.a) // Not found on top-level.
    }

    // Unqualified lookups should find the most local CodingKeys type available.
    public func nestedUnqualifiedFoo(_ key: CodingKeys) {
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func foo(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      foo(CodingKeys.nested)
    }

    internal func nestedUnqualifiedBar(_ key: CodingKeys) {
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func bar(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      bar(CodingKeys.nested)
    }

    fileprivate func nestedUnqualifiedBaz(_ key: CodingKeys) {
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func baz(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      baz(CodingKeys.nested)
    }

    private func nestedUnqualifiedQux(_ key: CodingKeys) {
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func qux(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      qux(CodingKeys.nested)
    }
  }
}

// MARK: - CodingKeys Enums in Extensions

// Structs which get a CodingKeys type in an extension should be able to see
// that type during member type lookup.
struct ExtendedStruct : Codable {
  let value: String = "foo"

  // Don't get an auto-synthesized type.
  init(from decoder: Decoder) throws {}
  func encode(to encoder: Encoder) throws {}

  // Qualified type lookup should always be unambiguous.
  public func qualifiedFoo(_ key: ExtendedStruct.CodingKeys) {}
  internal func qualifiedBar(_ key: ExtendedStruct.CodingKeys) {}
  fileprivate func qualfiedBaz(_ key: ExtendedStruct.CodingKeys) {}
  private func qualifiedQux(_ key: ExtendedStruct.CodingKeys) {}

  // Unqualified lookups should find the synthesized CodingKeys type instead
  // of the top-level type above.
  public func unqualifiedFoo(_ key: CodingKeys) {
    print(CodingKeys.a) // Not found on top-level.
  }

  internal func unqualifiedBar(_ key: CodingKeys) {
    print(CodingKeys.a) // Not found on top-level.
  }

  fileprivate func unqualifiedBaz(_ key: CodingKeys) {
    print(CodingKeys.a) // Not found on top-level.
  }

  private func unqualifiedQux(_ key: CodingKeys) {
    print(CodingKeys.a) // Not found on top-level.
  }

  // Unqualified lookups should find the most local CodingKeys type available.
  public func nestedUnqualifiedFoo(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func foo(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    foo(CodingKeys.nested)
  }

  internal func nestedUnqualifiedBar(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func bar(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    bar(CodingKeys.nested)
  }

  fileprivate func nestedUnqualifiedBaz(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func baz(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    baz(CodingKeys.nested)
  }

  private func nestedUnqualifiedQux(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func qux(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    qux(CodingKeys.nested)
  }

  // Lookup within nested types should look outside of the type.
  struct Nested {
    // Qualified lookup should remain as-is.
    public func qualifiedFoo(_ key: ExtendedStruct.CodingKeys) {}
    internal func qualifiedBar(_ key: ExtendedStruct.CodingKeys) {}
    fileprivate func qualfiedBaz(_ key: ExtendedStruct.CodingKeys) {}
    private func qualifiedQux(_ key: ExtendedStruct.CodingKeys) {}

    // Unqualified lookups should find the ExtendedStruct's synthesized
    // CodingKeys type instead of the top-level type above.
    public func unqualifiedFoo(_ key: CodingKeys) {
      print(CodingKeys.a) // Not found on top-level.
    }

    internal func unqualifiedBar(_ key: CodingKeys) {
      print(CodingKeys.a) // Not found on top-level.
    }

    fileprivate func unqualifiedBaz(_ key: CodingKeys) {
      print(CodingKeys.a) // Not found on top-level.
    }

    private func unqualifiedQux(_ key: CodingKeys) {
      print(CodingKeys.a) // Not found on top-level.
    }

    // Unqualified lookups should find the most local CodingKeys type available.
    public func nestedUnqualifiedFoo(_ key: CodingKeys) {
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func foo(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      foo(CodingKeys.nested)
    }

    internal func nestedUnqualifiedBar(_ key: CodingKeys) {
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func bar(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      bar(CodingKeys.nested)
    }

    fileprivate func nestedUnqualifiedBaz(_ key: CodingKeys) {
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func baz(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      baz(CodingKeys.nested)
    }

    private func nestedUnqualifiedQux(_ key: CodingKeys) {
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func qux(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      qux(CodingKeys.nested)
    }
  }
}

extension ExtendedStruct {
  enum CodingKeys : String, CodingKey {
    case a, b, c
  }
}

struct A {
  struct Inner : Codable {
    var value: Int = 42

    func foo() {
      print(CodingKeys.value) // Not found on A.CodingKeys or top-level type.
    }
  }
}

extension A {
  enum CodingKeys : String, CodingKey {
    case a
  }
}

struct B : Codable {
  // So B conforms to Codable using CodingKeys.b below.
  var b: Int = 0

  struct Inner {
    var value: Int = 42

    func foo() {
      print(CodingKeys.b) // Not found on top-level type.
    }
  }
}

extension B {
  enum CodingKeys : String, CodingKey {
    case b
  }
}

struct C : Codable {
  struct Inner : Codable {
    var value: Int = 42

    func foo() {
      print(CodingKeys.value) // Not found on C.CodingKeys or top-level type.
    }
  }
}

extension C.Inner {
  enum CodingKeys : String, CodingKey {
    case value
  }
}

struct GenericCodableStruct<T : Codable> : Codable {}

func foo(_: GenericCodableStruct<Int>.CodingKeys) // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}

struct sr6886 {
  struct Nested : Codable {}
  let Nested: Nested // Don't crash with a coding key that is the same as a nested type name
}

// Don't crash if we have a static property with the same name as an ivar that
// we will encode. We check the actual codegen in a SILGen test.
struct StaticInstanceNameDisambiguation : Codable {
  static let version: Float = 0.42
  let version: Int = 42
}
