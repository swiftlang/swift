// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// A top-level CodingKeys type to fall back to in lookups below.
public enum CodingKeys : String, CodingKey {
  case topLevel
}

// MARK: - Synthesized CodingKeys Enum

// Enums which get synthesized Codable implementations should have visible
// CodingKey enums during member type lookup.
enum SynthesizedEnum : Codable {
  case value

  // Qualified type lookup should always be unambiguous.
  public func qualifiedFoo(_ key: SynthesizedEnum.CodingKeys) {} // expected-error {{method cannot be declared public because its parameter uses a private type}}
  internal func qualifiedBar(_ key: SynthesizedEnum.CodingKeys) {} // expected-error {{method cannot be declared internal because its parameter uses a private type}}
  fileprivate func qualfiedBaz(_ key: SynthesizedEnum.CodingKeys) {} // expected-error {{method cannot be declared fileprivate because its parameter uses a private type}}
  private func qualifiedQux(_ key: SynthesizedEnum.CodingKeys) {}

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
    public func qualifiedFoo(_ key: SynthesizedEnum.CodingKeys) {} // expected-error {{method cannot be declared public because its parameter uses a private type}}
    internal func qualifiedBar(_ key: SynthesizedEnum.CodingKeys) {} // expected-error {{method cannot be declared internal because its parameter uses a private type}}
    fileprivate func qualfiedBaz(_ key: SynthesizedEnum.CodingKeys) {} // expected-error {{method cannot be declared fileprivate because its parameter uses a private type}}
    private func qualifiedQux(_ key: SynthesizedEnum.CodingKeys) {}

    // Unqualified lookups should find the SynthesizedEnum's synthesized
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

// Enums which don't get synthesized Codable implementations should expose the
// appropriate CodingKeys type.
enum NonSynthesizedEnum : Codable { // expected-note 4 {{'NonSynthesizedEnum' declared here}}
  case value

  // No synthesized type since we implemented both methods.
  init(from decoder: Decoder) throws {}
  func encode(to encoder: Encoder) throws {}

  // Qualified type lookup should clearly fail -- we shouldn't get a synthesized
  // type here.
  public func qualifiedFoo(_ key: NonSynthesizedEnum.CodingKeys) {} // expected-error {{'CodingKeys' is not a member type of enum 'enum_codable_member_type_lookup.NonSynthesizedEnum'}}
  internal func qualifiedBar(_ key: NonSynthesizedEnum.CodingKeys) {} // expected-error {{'CodingKeys' is not a member type of enum 'enum_codable_member_type_lookup.NonSynthesizedEnum'}}
  fileprivate func qualfiedBaz(_ key: NonSynthesizedEnum.CodingKeys) {} // expected-error {{'CodingKeys' is not a member type of enum 'enum_codable_member_type_lookup.NonSynthesizedEnum'}}
  private func qualifiedQux(_ key: NonSynthesizedEnum.CodingKeys) {} // expected-error {{'CodingKeys' is not a member type of enum 'enum_codable_member_type_lookup.NonSynthesizedEnum'}}

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

// Enums which explicitly define their own CodingKeys types should have
// visible CodingKey enums during member type lookup.
enum ExplicitStruct : Codable {
  case value

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

// Enumss which get a CodingKeys type in an extension should be able to see
// that type during member type lookup.
enum ExtendedEnum : Codable {
  case value

  // Don't get an auto-synthesized type.
  init(from decoder: Decoder) throws {}
  func encode(to encoder: Encoder) throws {}

  // Qualified type lookup should always be unambiguous.
  public func qualifiedFoo(_ key: ExtendedEnum.CodingKeys) {}
  internal func qualifiedBar(_ key: ExtendedEnum.CodingKeys) {}
  fileprivate func qualfiedBaz(_ key: ExtendedEnum.CodingKeys) {}
  private func qualifiedQux(_ key: ExtendedEnum.CodingKeys) {}

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
    public func qualifiedFoo(_ key: ExtendedEnum.CodingKeys) {}
    internal func qualifiedBar(_ key: ExtendedEnum.CodingKeys) {}
    fileprivate func qualfiedBaz(_ key: ExtendedEnum.CodingKeys) {}
    private func qualifiedQux(_ key: ExtendedEnum.CodingKeys) {}

    // Unqualified lookups should find the ExtendedEnum's synthesized
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

extension ExtendedEnum {
  enum CodingKeys : String, CodingKey {
    case a, b, c
  }
}

struct A {
  enum Inner : Codable {
    case value

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

enum B : Codable {
  // So B conforms to Codable using CodingKeys.b below.
  case b

  enum Inner {
    case value

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

enum C : Codable {
  enum Inner : Codable {
    case value

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

enum GenericCodableEnum<T : Codable> : Codable {}

func foo(_: GenericCodableEnum<Int>.CodingKeys) // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
