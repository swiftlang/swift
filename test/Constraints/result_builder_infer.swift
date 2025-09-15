// First, test everything together.
//
// RUN: %target-typecheck-verify-swift

// Now to the cross-module test. The result builder and protocols go to the
// module, the rest to the importing file.
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src
//
// RUN: %target-swift-frontend -emit-module -module-name M -emit-module-path %t/M.swiftmodule %t/src/M.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t -D CROSS_MODULE %t/src/test.swift

//--- M.swift

public struct Result {
  public init() {}
}

@resultBuilder
public enum Builder<T> {
  public static func buildBlock(_: T...) -> Result { Result() }
}

public protocol P_Builder_Int1 {
  @Builder<Int>
  func function() -> Result

  @Builder<Int>
  var property1: Result { get }
  var property2: Result { @Builder<Int> get }

  @Builder<Int>
  subscript(subscript1 _: Int) -> Result { get }
  subscript(subscript2 _: Int) -> Result { @Builder<Int> get }
}

public protocol P_Builder_Int2 {
  @Builder<Int>
  func function() -> Result

  @Builder<Int>
  var property1: Result { get }
  var property2: Result { @Builder<Int> get }

  @Builder<Int>
  subscript(subscript1 _: Int) -> Result { get }
  subscript(subscript2 _: Int) -> Result { @Builder<Int> get }
}

public protocol P_Builder_String {
  @Builder<String>
  func function() -> Result

  @Builder<String>
  var property1: Result { get }
  var property2: Result { @Builder<String> get }

  @Builder<String>
  subscript(subscript1 _: Int) -> Result { get }
  subscript(subscript2 _: Int) -> Result { @Builder<String> get }
}

public protocol P_Builder_Bool {
  @Builder<Bool>
  func function() -> Result

  @Builder<Bool>
  var property1: Result { get }
  var property2: Result { @Builder<Bool> get }

  @Builder<Bool>
  subscript(subscript1 _: Int) -> Result { get }
  subscript(subscript2 _: Int) -> Result { @Builder<Bool> get }
}

//--- test.swift

#if CROSS_MODULE
import M
#endif

// Result builder cannot be inferred from overridden declaration.
do {
  class Super {
    @Builder<Int> func function() -> Result { 1 }
  }
  class Sub: Super {
    override public func function() -> Result { Result() }
  }
}

// Result builder of dynamic replacement can be inferred from the replaced
// declaration.
struct Test1 {
  @Builder<Int>
  dynamic func function() -> Result { 1 }

  @Builder<Int>
  dynamic var property1: Result { 1 }
  dynamic var property2: Result { @Builder<Int> get { 1 } }

  @Builder<Int>
  dynamic subscript(subscript1 _: Int) -> Result { 1 }
  dynamic subscript(subscript2 _: Int) -> Result { @Builder<Int> get { 1 } }
}
extension Test1 {
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { 1 }

  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { 1 }
  @_dynamicReplacement(for: property2)
  var replacement_property2: Result { 1 }

  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result { 1 }
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { 1 }
}

// Edge case: context is a protocol extension.
protocol Test1_1 {}
extension Test1_1 {
  @Builder<Int>
  dynamic func function() -> Result { 1 }

  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { 1 }
}

// Result builders can be inferred from protocol requirements.
do {
  struct Test: P_Builder_Int1 {
    func function() -> Result { 1 }

    var property1: Result { 1 }
    var property2: Result { 1 }

    subscript(subscript1 _: Int) -> Result { 1 }
    subscript(subscript2 _: Int) -> Result { 1 }
  }
}

// Exception: inference does not support function parameters.
do {
  protocol P {
    func function(@Builder<Int> _: () -> Result)
  }
  struct Test: P {
    func function(_: () -> Result) {}
  }

  Test().function { Result() }
}

// Result builder of a dynamic replacement can be
// inferred from a protocol requirement that is witnessed by the replaced
// declaration.
struct Test2: P_Builder_Int1 {
  dynamic func function() -> Result { 1 }

  dynamic var property1: Result { 1 }
  dynamic var property2: Result { 1 }

  dynamic subscript(subscript1 _: Int) -> Result { 1 }
  dynamic subscript(subscript2 _: Int) -> Result { 1 }
}
extension Test2 {
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { 1 }

  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { 1 }
  @_dynamicReplacement(for: property2)
  var replacement_property2: Result { 1 }

  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result { 1 }
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { 1 }
}

// Inference from multiple conflicting sources with matching result builder
// types is unambiguous.

struct Test3: P_Builder_Int1, P_Builder_Int2 {
  dynamic func function() -> Result { 1 }

  dynamic var property1: Result { 1 }
  dynamic var property2: Result { 1 }

  dynamic subscript(subscript1 _: Int) -> Result { 1 }
  dynamic subscript(subscript2 _: Int) -> Result { 1 }
}
extension Test3 {
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { 1 }

  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { 1 }
  @_dynamicReplacement(for: property2)
  var replacement_property2: Result { 1 }

  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result { 1 }
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { 1 }
}

struct Test4 {
  @Builder<Int> dynamic func replaced_function() -> Result { 1 }

  @Builder<Int> dynamic var replaced_property1: Result { 1 }
  @Builder<Int> dynamic var replaced_property2: Result { 1 }

  @Builder<Int> dynamic subscript(replaced_subscript1 _: Int) -> Result { 1 }
  @Builder<Int> dynamic subscript(replaced_subscript2 _: Int) -> Result { 1 }
}
extension Test4: P_Builder_Int1 {
  @_dynamicReplacement(for: replaced_function)
  func function() -> Result { 1 }

  @_dynamicReplacement(for: replaced_property1)
  var property1: Result { 1 }
  @_dynamicReplacement(for: replaced_property2)
  var property2: Result { 1 }

  @_dynamicReplacement(for: subscript(replaced_subscript1:))
  subscript(subscript1 _: Int) -> Result { 1 }
  @_dynamicReplacement(for: subscript(replaced_subscript2:))
  subscript(subscript2 _: Int) -> Result { 1 }
}

// Ambiguous inference.

struct Test5: P_Builder_Int1, P_Builder_Int2, P_Builder_String {
  // expected-note@+6{{add an explicit 'return' statement to not use a result builder}}{{+1:3-3=return <#expr#>\n}}
  // expected-note@+5{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{-1:3-3=@Builder<Int> }}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int2')}}{{-1:3-3=@Builder<Int> }} // FIXME: This is redundant; we already suggested Builder<Int>.
  // expected-note@+3{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{-1:3-3=@Builder<String> }}
  // expected-error@+2{{ambiguous result builder inferred for 'function()': 'Builder<Int>' or 'Builder<String>'}}
  internal dynamic
  func function() -> Result {
  }

  // FIXME: 'return' will be inserted without semicolon.
  // expected-note@+6{{add an explicit 'return' statement to not use a result builder}}{{13-13=return <#expr#>\n}}
  // expected-note@+5{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{-1:3-3=@Builder<Int> }}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int2')}}{{-1:3-3=@Builder<Int> }} // FIXME: This is redundant; we already suggested Builder<Int>.
  // expected-note@+3{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{-1:3-3=@Builder<String> }}
  // expected-error@+2{{ambiguous result builder inferred for 'property1': 'Builder<Int>' or 'Builder<String>'}}
  dynamic var property1: Result {
    get { 1 } // expected-error {{cannot convert return expression}}
  }

  // expected-note@+6{{add an explicit 'return' statement to not use a result builder}}{{10-10=return <#expr#>\n}}
  // expected-note@+5{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{-1:3-3=@Builder<Int> }}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int2')}}{{-1:3-3=@Builder<Int> }} // FIXME: This is redundant; we already suggested Builder<Int>.
  // expected-note@+3{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{-1:3-3=@Builder<String> }}
  // expected-error@+2{{ambiguous result builder inferred for 'property2': 'Builder<Int>' or 'Builder<String>'}}
  dynamic var property2: Result {
    get {}
  }

  // expected-note@+5{{add an explicit 'return' statement to not use a result builder}}{{51-51=return <#expr#>\n}}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{3-3=@Builder<Int> }}
  // expected-note@+3{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int2')}}{{3-3=@Builder<Int> }} // FIXME: This is redundant; we already suggested Builder<Int>.
  // expected-note@+2{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{3-3=@Builder<String> }}
  // expected-error@+1{{ambiguous result builder inferred for 'subscript(subscript1:)': 'Builder<Int>' or 'Builder<String>'}}
  dynamic subscript(subscript1 _: Int) -> Result {}

  // expected-note@+5{{add an explicit 'return' statement to not use a result builder}}{{+1:3-3=return <#expr#>\n}}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{3-3=@Builder<Int> }}
  // expected-note@+3{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int2')}}{{3-3=@Builder<Int> }} // FIXME: This is redundant; we already suggested Builder<Int>.
  // expected-note@+2{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{3-3=@Builder<String> }}
  // expected-error@+1{{ambiguous result builder inferred for 'subscript(subscript2:)': 'Builder<Int>' or 'Builder<String>'}}
  dynamic subscript(subscript2 _: Int) -> Result {
  }
}
extension Test5 {
  // expected-note@+6{{add an explicit 'return' statement to not use a result builder}}{{+1:3-3=return <#expr#>\n}}
  // expected-note@+5{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{-1:3-3=@Builder<Int> }}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int2')}}{{-1:3-3=@Builder<Int> }} // FIXME: This is redundant; we already suggested Builder<Int>.
  // expected-note@+3{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{-1:3-3=@Builder<String> }}
  // expected-error@+2{{ambiguous result builder inferred for 'replacement_function()': 'Builder<Int>' or 'Builder<String>'}}
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result {
  }

  // expected-note@+6{{add an explicit 'return' statement to not use a result builder}}{{+1:3-3=return <#expr#>\n}}
  // expected-note@+5{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{-1:3-3=@Builder<Int> }}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int2')}}{{-1:3-3=@Builder<Int> }} // FIXME: This is redundant; we already suggested Builder<Int>.
  // expected-note@+3{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{-1:3-3=@Builder<String> }}
  // expected-error@+2{{ambiguous result builder inferred for 'replacement_property1': 'Builder<Int>' or 'Builder<String>'}}
  @_dynamicReplacement(for: property1)
  var replacement_property1: Result {
  }

  // expected-note@+6{{add an explicit 'return' statement to not use a result builder}}{{+1:3-3=return <#expr#>\n}}
  // expected-note@+5{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{-1:3-3=@Builder<Int> }}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int2')}}{{-1:3-3=@Builder<Int> }} // FIXME: This is redundant; we already suggested Builder<Int>.
  // expected-note@+3{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{-1:3-3=@Builder<String> }}
  // expected-error@+2{{ambiguous result builder inferred for 'replacement_property2': 'Builder<Int>' or 'Builder<String>'}}
  @_dynamicReplacement(for: property2)
  var replacement_property2: Result {
  }

  // expected-note@+6{{add an explicit 'return' statement to not use a result builder}}{{+1:3-3=return <#expr#>\n}}
  // expected-note@+5{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{-1:3-3=@Builder<Int> }}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int2')}}{{-1:3-3=@Builder<Int> }} // FIXME: This is redundant; we already suggested Builder<Int>.
  // expected-note@+3{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{-1:3-3=@Builder<String> }}
  // expected-error@+2{{ambiguous result builder inferred for 'subscript(replacement_subscript1:)': 'Builder<Int>' or 'Builder<String>'}}
  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result {
  }

  // expected-note@+6{{add an explicit 'return' statement to not use a result builder}}{{+1:3-3=return <#expr#>\n}}
  // expected-note@+5{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{-1:3-3=@Builder<Int> }}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int2')}}{{-1:3-3=@Builder<Int> }} // FIXME: This is redundant; we already suggested Builder<Int>.
  // expected-note@+3{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{-1:3-3=@Builder<String> }}
  // expected-error@+2{{ambiguous result builder inferred for 'subscript(replacement_subscript2:)': 'Builder<Int>' or 'Builder<String>'}}
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result {
  }
}

struct Test6 {}
extension Test6: P_Builder_Int1, P_Builder_String {
  // expected-note@+6{{add an explicit 'return' statement to not use a result builder}}{{+1:3-3=return <#expr#>\n}}
  // expected-note@+5{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{-1:3-3=@Builder<String> }}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{-1:3-3=@Builder<Int> }}
  // expected-note@+3{{apply result builder 'Builder<Bool>' (inferred from dynamic replacement of 'replaced_function()')}}{{-1:3-3=@Builder<Bool> }}
  // expected-error@+2{{ambiguous result builder inferred for 'function()': 'Builder<Int>' or 'Builder<String>'}}
  @_dynamicReplacement(for: replaced_function)
  func function() -> Result {
  }

  // expected-note@+6{{add an explicit 'return' statement to not use a result builder}}{{+1:3-3=return <#expr#>\n}}
  // expected-note@+5{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{-1:3-3=@Builder<String> }}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{-1:3-3=@Builder<Int> }}
  // expected-note@+3{{apply result builder 'Builder<Bool>' (inferred from dynamic replacement of 'replaced_property1')}}{{-1:3-3=@Builder<Bool> }}
  // expected-error@+2{{ambiguous result builder inferred for 'property1': 'Builder<Int>' or 'Builder<String>'}}
  @_dynamicReplacement(for: replaced_property1)
  var property1: Result {
  }

  // expected-note@+6{{add an explicit 'return' statement to not use a result builder}}{{+1:3-3=return <#expr#>\n}}
  // expected-note@+5{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{-1:3-3=@Builder<String> }}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{-1:3-3=@Builder<Int> }}
  // expected-note@+3{{apply result builder 'Builder<Bool>' (inferred from dynamic replacement of 'replaced_property2')}}{{-1:3-3=@Builder<Bool> }}
  // expected-error@+2{{ambiguous result builder inferred for 'property2': 'Builder<Int>' or 'Builder<String>'}}
  @_dynamicReplacement(for: replaced_property2)
  var property2: Result {
  }

  // expected-note@+6{{add an explicit 'return' statement to not use a result builder}}{{+1:3-3=return <#expr#>\n}}
  // expected-note@+5{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{-1:3-3=@Builder<String> }}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{-1:3-3=@Builder<Int> }}
  // expected-note@+3{{apply result builder 'Builder<Bool>' (inferred from dynamic replacement of 'subscript(replaced_subscript1:)')}}{{-1:3-3=@Builder<Bool> }}
  // expected-error@+2{{ambiguous result builder inferred for 'subscript(subscript1:)': 'Builder<Int>' or 'Builder<String>'}}
  @_dynamicReplacement(for: subscript(replaced_subscript1:))
  subscript(subscript1 _: Int) -> Result {
  }

  // expected-note@+6{{add an explicit 'return' statement to not use a result builder}}{{+1:3-3=return <#expr#>\n}}
  // expected-note@+5{{apply result builder 'Builder<String>' (inferred from protocol 'P_Builder_String')}}{{-1:3-3=@Builder<String> }}
  // expected-note@+4{{apply result builder 'Builder<Int>' (inferred from protocol 'P_Builder_Int1')}}{{-1:3-3=@Builder<Int> }}
  // expected-note@+3{{apply result builder 'Builder<Bool>' (inferred from dynamic replacement of 'subscript(replaced_subscript2:)')}}{{-1:3-3=@Builder<Bool> }}
  // expected-error@+2{{ambiguous result builder inferred for 'subscript(subscript2:)': 'Builder<Int>' or 'Builder<String>'}}
  @_dynamicReplacement(for: subscript(replaced_subscript2:))
  subscript(subscript2 _: Int) -> Result {
  }
}
extension Test6 {
  @Builder<Bool> dynamic func replaced_function() -> Result { true }

  @Builder<Bool> dynamic var replaced_property1: Result { true }
  @Builder<Bool> dynamic var replaced_property2: Result { true }

  @Builder<Bool> dynamic subscript(replaced_subscript1 _: Int) -> Result { true }
  @Builder<Bool> dynamic subscript(replaced_subscript2 _: Int) -> Result { true }
}

// (?) Overridden result builders in protocol hierarchies still contribute to
// ambiguities.
do {
  protocol P1 {
    @Builder<Int>
    func function() -> Result
  }
  protocol P2: P1 {
    @Builder<String>
    func function() -> Result
  }
  protocol P3: P2 {
    @Builder<Bool>
    func function() -> Result
  }

  struct S: P3 {
    // expected-note@+5{{add an explicit 'return' statement to not use a result builder}}{{+1:5-5=return <#expr#>\n}}
    // expected-note@+4{{apply result builder 'Builder<Bool>' (inferred from protocol 'P3')}}{{5-5=@Builder<Bool> }}
    // expected-note@+3{{apply result builder 'Builder<Int>' (inferred from protocol 'P1')}}{{5-5=@Builder<Int> }}
    // expected-note@+2{{apply result builder 'Builder<String>' (inferred from protocol 'P2')}}{{5-5=@Builder<String> }}
    // expected-error@+1{{ambiguous result builder inferred for 'function()': 'Builder<Bool>' or 'Builder<String>'}}
    func function() -> Result {
    }
  }
}

//==============================================================================
// Inference suppression: explicit result builder.
//==============================================================================

struct Test7 {
  @Builder<Int> dynamic func function() -> Result { 1 }

  @Builder<Int> 
  dynamic var property1: Result { 1 }
  dynamic var property2: Result { @Builder<Int> get { 1 } }

  @Builder<Int> 
  dynamic subscript(subscript1 _: Int) -> Result { 1 }
  dynamic subscript(subscript2 _: Int) -> Result { @Builder<Int> get { 1 } }
}
extension Test7 {
  @Builder<Bool>
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { true }

  @Builder<Bool>
  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { true }

  @_dynamicReplacement(for: property2)
  var replacement_property2: Result {
    @Builder<Bool> get { true }
  }

  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result {
    @Builder<Bool> get { true }
  }

  @Builder<Bool>
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { true }
}

struct Test8: P_Builder_Int1 {
  dynamic func function() -> Result { 1 }

  dynamic var property1: Result { 1 }
  dynamic var property2: Result { 1 }

  dynamic subscript(subscript1 _: Int) -> Result { 1 }
  dynamic subscript(subscript2 _: Int) -> Result { 1 }
}
extension Test8 {
  @Builder<Bool>
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { true }

  @Builder<Bool>
  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { true }
  @_dynamicReplacement(for: property2)
  var replacement_property2: Result { @Builder<Bool> get { true } }

  @Builder<Bool>
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { true }
  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result { @Builder<Bool> get { true } }
}

// Inference from dynamically replaced declaration is prioritized
// above inference from protocol requirements that are witnessed by it.
struct Test9: P_Builder_String {
  @Builder<Int>
  dynamic func function() -> Result { 1 }

  @Builder<Int>
  dynamic var property2: Result { 1 }
  dynamic var property1: Result { @Builder<Int> get { 1 } }

  @Builder<Int>
  dynamic subscript(subscript1 _: Int) -> Result { 1 }
  dynamic subscript(subscript2 _: Int) -> Result { @Builder<Int> get { 1 } }
}
extension Test9 {
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { 1 }

  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { 1 }
  @_dynamicReplacement(for: property2)
  var replacement_property2: Result { 1 }

  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result { 1 }
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { 1 }
}

struct Test10: P_Builder_Int1, P_Builder_String {
  @Builder<Int> dynamic func function() -> Result { 1 }

  @Builder<Int>
  dynamic var property1: Result { 1 }
  dynamic var property2: Result { @Builder<Int> get { 1 } }

  @Builder<Int>
  dynamic subscript(subscript1 _: Int) -> Result { 1 }
  dynamic subscript(subscript2 _: Int) -> Result { @Builder<Int> get { 1 } }
}
extension Test10 {
  @Builder<Bool>
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { true }

  @Builder<Bool>
  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { true }
  @_dynamicReplacement(for: property2)
  var replacement_property2: Result { @Builder<Bool> get { true } }

  @Builder<Bool>
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { true }
  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result { @Builder<Bool> get { true } }
}

//==============================================================================
// Inference suppression: return statements.
//==============================================================================

struct Test11 {
  @Builder<Int>
  dynamic func function() -> Result { 1 }

  @Builder<Int>
  dynamic var property1: Result { 1 }
  dynamic var property2: Result { @Builder<Int> get { 1 } }

  @Builder<Int>
  dynamic subscript(subscript1 _: Int) -> Result { 1 }
  dynamic subscript(subscript2 _: Int) -> Result { @Builder<Int> get { 1 } }
}
extension Test11 {
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result {
    do { if true { while true { return Result() } } }
  }

  @_dynamicReplacement(for: property2)
  var replacement_property2: Result { return Result() }
  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { get { return Result() } }

  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result { return Result() }
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { get { return Result() } }
}

struct Test12: P_Builder_Int1 {
  dynamic func function() -> Result { 1 }

  dynamic var property1: Result { 1 }
  dynamic var property2: Result { 1 }

  dynamic subscript(subscript1 _: Int) -> Result { 1 }
  dynamic subscript(subscript2 _: Int) -> Result { 1 }
}
extension Test12 {
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { return Result() }

  @_dynamicReplacement(for: property2)
  var replacement_property2: Result { return Result() }
  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { get { return Result() } }

  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result { return Result() }
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { get { return Result() } }
}

// Return statement in dynamically replaced declaration does not prevent the
// replacement from using it to infer a result builder.

struct Test13 {
  // @expected-note@+1 {{remove the attribute to explicitly disable the result builder}}
  @Builder<Int>
  dynamic func function() -> Result {
    return Result()
    // @expected-warning@-1 {{application of result builder 'Builder<Int>' disabled by explicit 'return' statement}}
    // @expected-note@-2 {{remove 'return' statements to apply the result builder}}
  }

  // @expected-note@+1 {{remove the attribute to explicitly disable the result builder}}
  @Builder<Int>
  dynamic var property1: Result {
    return Result()
    // @expected-warning@-1 {{application of result builder 'Builder<Int>' disabled by explicit 'return' statement}}
    // @expected-note@-2 {{remove 'return' statements to apply the result builder}}
  }
  dynamic var property2: Result {
    // @expected-note@+1 {{remove the attribute to explicitly disable the result builder}}
    @Builder<Int>
    get {
      return Result()
      // @expected-warning@-1 {{application of result builder 'Builder<Int>' disabled by explicit 'return' statement}}
      // @expected-note@-2 {{remove 'return' statements to apply the result builder}}
    }
  }

  // @expected-note@+1 {{remove the attribute to explicitly disable the result builder}}
  @Builder<Int>
  dynamic subscript(subscript1 _: Int) -> Result {
    return Result()
    // @expected-warning@-1 {{application of result builder 'Builder<Int>' disabled by explicit 'return' statement}}
    // @expected-note@-2 {{remove 'return' statements to apply the result builder}}
  }
  dynamic subscript(subscript2 _: Int) -> Result {
    // @expected-note@+1 {{remove the attribute to explicitly disable the result builder}}
    @Builder<Int> get {
      return Result()
      // @expected-warning@-1 {{application of result builder 'Builder<Int>' disabled by explicit 'return' statement}}
      // @expected-note@-2 {{remove 'return' statements to apply the result builder}}
    }
  }
}
extension Test13 {
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { 1 }

  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { 1 }
  @_dynamicReplacement(for: property2)
  var replacement_property2: Result { 1 }

  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result { 1 }
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { 1 }
}

struct Test14: P_Builder_Int1 {
  dynamic func function() -> Result { return Result() }

  dynamic var property1: Result { return Result() }
  dynamic var property2: Result { get { return Result() } }

  dynamic subscript(subscript1 _: Int) -> Result { return Result() }
  dynamic subscript(subscript2 _: Int) -> Result { get { return Result() } }
}
extension Test14 {
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { 1 }

  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { 1 }
  @_dynamicReplacement(for: property2)
  var replacement_property2: Result { 1 }

  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result { 1 }
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { 1 }
}

struct Test15: P_Builder_Int1, P_Builder_String {
  dynamic func function() -> Result { return Result() }

  dynamic var property1: Result { return Result() }
  dynamic var property2: Result { get { return Result() } }

  dynamic subscript(subscript1 _: Int) -> Result { return Result() }
  dynamic subscript(subscript2 _: Int) -> Result { get { return Result() } }
}
extension Test15 {
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { return Result() }

  @_dynamicReplacement(for: property2)
  var replacement_property2: Result { return Result() }
  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { get { return Result() } }

  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result { return Result() }
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { get { return Result() } }
}

//==============================================================================
// Inference suppression: separation of conformance context & witness context.
// Only conformances declared on the witness context are considered in
// inference from protocol requirements.
//==============================================================================

struct Test16 {
  dynamic func function() -> Result { Result() }

  dynamic var property1: Result { Result() }
  dynamic var property2: Result { Result() }

  dynamic subscript(subscript1 _: Int) -> Result { Result() }
  dynamic subscript(subscript2 _: Int) -> Result { Result() }
}
extension Test16: P_Builder_Int1 {
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { Result() }

  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { Result() }
  @_dynamicReplacement(for: property2)
  var replacement_property2: Result { Result() }

  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result { Result() }
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { Result() }
}

struct Test17 {
  dynamic func function() -> Result { Result() }
}
extension Test17: P_Builder_Int1 {
  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { Result() }

  dynamic var property1: Result { 1 }
}
extension Test17: P_Builder_String {
  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { 1 }

  dynamic var property2: Result { "1" }
}
extension Test17: P_Builder_Bool {
  @_dynamicReplacement(for: property2)
  var replacement_property2: Result { "1" }

  dynamic subscript(subscript1 _: Int) -> Result { true }
}
extension Test17: P_Builder_Int2 {
  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result { true }

  dynamic subscript(subscript2 _: Int) -> Result { 1 }
}
extension Test17 {
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { 1 }
}

struct Test18: P_Builder_Int1, P_Builder_String, P_Builder_Bool {}
extension Test18 {
  dynamic func function() -> Result { Result() }

  dynamic var property1: Result { Result() }
  dynamic var property2: Result { Result() }

  dynamic subscript(subscript1 _: Int) -> Result { Result() }
  dynamic subscript(subscript2 _: Int) -> Result { Result() }

  @_dynamicReplacement(for: function)
  func replacement_function() -> Result { Result() }

  @_dynamicReplacement(for: property1)
  var replacement_property1: Result { Result() }
  @_dynamicReplacement(for: property2)
  var replacement_property2: Result { Result() }

  @_dynamicReplacement(for: subscript(subscript1:))
  subscript(replacement_subscript1 _: Int) -> Result { Result() }
  @_dynamicReplacement(for: subscript(subscript2:))
  subscript(replacement_subscript2 _: Int) -> Result { Result() }
}
