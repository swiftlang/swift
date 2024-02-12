// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -package-name myPkg

// A top-level CodingKeys type to fall back to in lookups below.
public enum CodingKeys : String, CodingKey {
  case topLevel
}

// MARK: - Synthesized CodingKeys Enum

// Classes which get synthesized Codable implementations should have visible
// CodingKey enums during member type lookup.
struct SynthesizedClass : Codable {
  let value: String = "foo"
  // expected-warning@-1 {{immutable property will not be decoded because it is declared with an initial value which cannot be overwritten}}
  // expected-note@-2 {{set the initial value via the initializer or explicitly define a CodingKeys enum including a 'value' case to silence this warning}}
  // expected-note@-3 {{make the property mutable instead}}{{3-6=var}}

  // Qualified type lookup should always be unambiguous.
  public func qualifiedFoo(_ key: SynthesizedClass.CodingKeys) {} // expected-error {{method cannot be declared public because its parameter uses a private type}}
  package func qualifiedPkg(_ key: SynthesizedClass.CodingKeys) {} // expected-error {{method cannot be declared package because its parameter uses a private type}}
  internal func qualifiedBar(_ key: SynthesizedClass.CodingKeys) {} // expected-error {{method cannot be declared internal because its parameter uses a private type}}
  fileprivate func qualifiedBaz(_ key: SynthesizedClass.CodingKeys) {} // expected-error {{method cannot be declared fileprivate because its parameter uses a private type}}
  private func qualifiedQux(_ key: SynthesizedClass.CodingKeys) {}

  // Unqualified lookups should find the synthesized CodingKeys type instead
  // of the top-level type above.
  public func unqualifiedFoo(_ key: CodingKeys) { // expected-error {{method cannot be declared public because its parameter uses a private type}}
    print(CodingKeys.value) // Not found on top-level.
  }

  package func unqualifiedPkg(_ key: CodingKeys) { // expected-error {{method cannot be declared package because its parameter uses a private type}}
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

  package func nestedUnqualifiedPkg(_ key: CodingKeys) { // expected-error {{method cannot be declared package because its parameter uses a private type}}
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func pkg(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    pkg(CodingKeys.nested)
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
    public func qualifiedFoo(_ key: SynthesizedClass.CodingKeys) {} // expected-error {{method cannot be declared public because its parameter uses a private type}}
    package func qualifiedPkg(_ key: SynthesizedClass.CodingKeys) {} // expected-error {{method cannot be declared package because its parameter uses a private type}}
    internal func qualifiedBar(_ key: SynthesizedClass.CodingKeys) {} // expected-error {{method cannot be declared internal because its parameter uses a private type}}
    fileprivate func qualifiedBaz(_ key: SynthesizedClass.CodingKeys) {} // expected-error {{method cannot be declared fileprivate because its parameter uses a private type}}
    private func qualifiedQux(_ key: SynthesizedClass.CodingKeys) {}

    // Unqualified lookups should find the SynthesizedClass's synthesized
    // CodingKeys type instead of the top-level type above.
    public func unqualifiedFoo(_ key: CodingKeys) { // expected-error {{method cannot be declared public because its parameter uses a private type}}
      print(CodingKeys.value) // Not found on top-level.
    }

    package func unqualifiedPkg(_ key: CodingKeys) { // expected-error {{method cannot be declared package because its parameter uses a private type}}
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

    package func nestedUnqualifiedPkg(_ key: CodingKeys) { // expected-error {{method cannot be declared package because its parameter uses a private type}}
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func pkg(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      pkg(CodingKeys.nested)
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

// Classes which don't get synthesized Codable implementations should expose the
// appropriate CodingKeys type.
struct NonSynthesizedClass : Codable { // expected-note * {{'NonSynthesizedClass' declared here}}
  // No synthesized type since we implemented both methods.
  init(from decoder: Decoder) throws {}
  func encode(to encoder: Encoder) throws {}

  // Qualified type lookup should clearly fail -- we shouldn't get a synthesized
  // type here.
  public func qualifiedFoo(_ key: NonSynthesizedClass.CodingKeys) {} // expected-error {{'CodingKeys' is not a member type of struct 'class_codable_member_type_lookup.NonSynthesizedClass'}}
  package func qualifiedPkg(_ key: NonSynthesizedClass.CodingKeys) {} // expected-error {{'CodingKeys' is not a member type of struct 'class_codable_member_type_lookup.NonSynthesizedClass'}}
  internal func qualifiedBar(_ key: NonSynthesizedClass.CodingKeys) {} // expected-error {{'CodingKeys' is not a member type of struct 'class_codable_member_type_lookup.NonSynthesizedClass'}}
  fileprivate func qualifiedBaz(_ key: NonSynthesizedClass.CodingKeys) {} // expected-error {{'CodingKeys' is not a member type of struct 'class_codable_member_type_lookup.NonSynthesizedClass'}}
  private func qualifiedQux(_ key: NonSynthesizedClass.CodingKeys) {} // expected-error {{'CodingKeys' is not a member type of struct 'class_codable_member_type_lookup.NonSynthesizedClass'}}

  // Unqualified lookups should find the public top-level CodingKeys type.
  public func unqualifiedFoo(_ key: CodingKeys) { print(CodingKeys.topLevel) }
  package func unqualifiedPkg(_ key: CodingKeys) { print(CodingKeys.topLevel) }
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

  package func nestedUnqualifiedPkg(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func pkg(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    pkg(CodingKeys.nested)
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

// Classes which explicitly define their own CodingKeys types should have
// visible CodingKey enums during member type lookup.
struct ExplicitClass : Codable {
  let value: String = "foo"

  public enum CodingKeys {
    case a
    case b
    case c
  }

  init(from decoder: Decoder) throws {}
  func encode(to encoder: Encoder) throws {}

  // Qualified type lookup should always be unambiguous.
  public func qualifiedFoo(_ key: ExplicitClass.CodingKeys) {}
  package func qualifiedPkg(_ key: ExplicitClass.CodingKeys) {}
  internal func qualifiedBar(_ key: ExplicitClass.CodingKeys) {}
  fileprivate func qualifiedBaz(_ key: ExplicitClass.CodingKeys) {}
  private func qualifiedQux(_ key: ExplicitClass.CodingKeys) {}

  // Unqualified lookups should find the synthesized CodingKeys type instead
  // of the top-level type above.
  public func unqualifiedFoo(_ key: CodingKeys) {
    print(CodingKeys.a) // Not found on top-level.
  }

  package func unqualifiedPkg(_ key: CodingKeys) {
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
  package func nestedUnqualifiedPkg(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func pkg(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    pkg(CodingKeys.nested)
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
    public func qualifiedFoo(_ key: ExplicitClass.CodingKeys) {}
    package func qualifiedPkg(_ key: ExplicitClass.CodingKeys) {}
    internal func qualifiedBar(_ key: ExplicitClass.CodingKeys) {}
    fileprivate func qualifiedBaz(_ key: ExplicitClass.CodingKeys) {}
    private func qualifiedQux(_ key: ExplicitClass.CodingKeys) {}

    // Unqualified lookups should find the ExplicitClass's synthesized
    // CodingKeys type instead of the top-level type above.
    public func unqualifiedFoo(_ key: CodingKeys) {
      print(CodingKeys.a) // Not found on top-level.
    }

    package func unqualifiedPkg(_ key: CodingKeys) {
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
    package func nestedUnqualifiedPkg(_ key: CodingKeys) {
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func pkg(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      pkg(CodingKeys.nested)
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

// Classes which get a CodingKeys type in an extension should be able to see
// that type during member type lookup.
struct ExtendedClass : Codable {
  let value: String = "foo"

  // Don't get an auto-synthesized type.
  init(from decoder: Decoder) throws {}
  func encode(to encoder: Encoder) throws {}

  // Qualified type lookup should always be unambiguous.
  public func qualifiedFoo(_ key: ExtendedClass.CodingKeys) {}
  package func qualifiedPkg(_ key: ExtendedClass.CodingKeys) {}
  internal func qualifiedBar(_ key: ExtendedClass.CodingKeys) {}
  fileprivate func qualifiedBaz(_ key: ExtendedClass.CodingKeys) {}
  private func qualifiedQux(_ key: ExtendedClass.CodingKeys) {}

  // Unqualified lookups should find the synthesized CodingKeys type instead
  // of the top-level type above.
  public func unqualifiedFoo(_ key: CodingKeys) {
    print(CodingKeys.a) // Not found on top-level.
  }

  package func unqualifiedPkg(_ key: CodingKeys) {
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
  package func nestedUnqualifiedPkg(_ key: CodingKeys) {
    enum CodingKeys : String, CodingKey {
      case nested
    }

    // CodingKeys should refer to the local unqualified enum.
    func pkg(_ key: CodingKeys) {
      print(CodingKeys.nested) // Not found on synthesized type or top-level type.
    }

    pkg(CodingKeys.nested)
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
    public func qualifiedFoo(_ key: ExtendedClass.CodingKeys) {}
    package func qualifiedPkg(_ key: ExtendedClass.CodingKeys) {}
    internal func qualifiedBar(_ key: ExtendedClass.CodingKeys) {}
    fileprivate func qualifiedBaz(_ key: ExtendedClass.CodingKeys) {}
    private func qualifiedQux(_ key: ExtendedClass.CodingKeys) {}

    // Unqualified lookups should find the ExtendedClass's synthesized
    // CodingKeys type instead of the top-level type above.
    public func unqualifiedFoo(_ key: CodingKeys) {
      print(CodingKeys.a) // Not found on top-level.
    }

    package func unqualifiedPkg(_ key: CodingKeys) {
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
    package func nestedUnqualifiedPkg(_ key: CodingKeys) {
      enum CodingKeys : String, CodingKey {
        case nested
      }

      // CodingKeys should refer to the local unqualified enum.
      func pkg(_ key: CodingKeys) {
        print(CodingKeys.nested) // Not found on synthesized type or top-level type.
      }

      pkg(CodingKeys.nested)
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

extension ExtendedClass {
  enum CodingKeys : String, CodingKey {
    case a, b, c
  }
}

class A {
  class Inner : Codable {
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

class B : Codable {
  // So B conforms to Codable using CodingKeys.b below.
  var b: Int = 0

  class Inner {
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

class C : Codable {
  class Inner : Codable {
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
