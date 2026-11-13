// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module-path %t/unsafe_swift_decls.swiftmodule %S/Inputs/unsafe_swift_decls.swift

// Strict memory safety diagnostics can be enabled with '@diagnose' without
// passing '-strict-memory-safety'.

// RUN: %target-swift-frontend -typecheck -verify %t/decl.swift
// RUN: %target-swift-frontend -typecheck -verify %t/file_default.swift -strict-concurrency=complete -I %t

// REQUIRES: concurrency

//--- decl.swift

@unsafe func unsafeFunc() { }

@unsafe(always) func alwaysUnsafeValue() -> Int { 0 }

@unsafe func unsafeValue() -> Int { 0 }

func takesTwo(_: Int, _: Int) { }

@unsafe class UnsafeBase { }

class SafeBase {
  func f() { }
  // expected-note@-1:8 {{overridden declaration is here}}
}

protocol P {
  func f()
}

struct UnsafeSequence: @unsafe IteratorProtocol, @unsafe Sequence {
  @unsafe mutating func next() -> Int? { nil }
}

// Everything here is diagnosed.
@diagnose(StrictMemorySafety, as: warning)
enum Enabled {
  struct Storage {
    // expected-warning@-1:10 {{struct 'Storage' has storage involving unsafe types}}{{documentation-file=strict-memory-safety}}
    // expected-note@-2:10 {{add '@unsafe' if this type is also unsafe to use}}{{3-3=@unsafe }}
    // expected-note@-3:10 {{add '@safe' if this type encapsulates the unsafe storage in a safe interface}}{{3-3=@safe }}
    var pointer: UnsafeMutablePointer<Int>
    // expected-note@-1:9 {{property 'pointer' involves unsafe type 'UnsafeMutablePointer<Int>'}}
  }

  class Subclass: UnsafeBase { }
  // expected-warning@-1:9 {{class 'Subclass' has superclass involving unsafe type 'UnsafeBase'}}{{3-3=@unsafe }}{{documentation-file=strict-memory-safety}}

  class Override: SafeBase {
    // expected-note@-1:9 {{make class 'Override' '@unsafe' to allow unsafe overrides of safe superclass methods}}{{3-3=@unsafe }}
    @unsafe override func f() { }
    // expected-warning@-1:27 {{override of safe instance method with unsafe instance method}}{{documentation-file=strict-memory-safety}}
  }

  struct Conformance: P {
    // expected-warning@-1:10 {{conformance of 'Enabled.Conformance' to protocol 'P' involves unsafe code; use '@unsafe' to indicate that the conformance is not memory-safe}}{{23-23=@unsafe }}{{documentation-file=strict-memory-safety}}
    @unsafe func f() { }
    // expected-note@-1:18 {{unsafe instance method 'f()' cannot satisfy safe requirement}}
  }

  static func expression() {
    unsafeFunc()
    // expected-warning@-1:5 {{expression uses unsafe constructs but is not marked with 'unsafe'}}{{5-5=unsafe }}{{documentation-file=strict-memory-safety}}
    // expected-note@-2:5 {{reference to unsafe global function 'unsafeFunc()'}}
  }

  static func forIn() {
    for _ in UnsafeSequence() { }
    // expected-warning@-1:5 {{for-in loop uses unsafe constructs but is not marked with 'unsafe'}}{{9-9=unsafe }}{{documentation-file=strict-memory-safety}}
    // expected-note@-2:5 {{'@unsafe' conformance of 'UnsafeSequence' to protocol 'Sequence' involves unsafe code}}
    // expected-note@-3:5 {{reference to unsafe instance method 'next()'}}
  }

  // Only the always-unsafe use is reported, whether or not strict memory
  // safety diagnostics are enabled.
  static func mixed() {
    takesTwo(alwaysUnsafeValue(), unsafeValue())
    // expected-error@-1:5 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{5-5=unsafe }}{{documentation-file=always-unsafe}}
    // expected-note@-2:14 {{reference to unsafe global function 'alwaysUnsafeValue()'}}
  }

  // Synthesized code covers its unsafe uses with 'unsafe' regardless of how
  // strict memory safety diagnostics were enabled.
  @unsafe struct UnsafeCodable: Codable, Hashable {
    var string: String
  }

  struct SafeCodable: Codable, Hashable {
    // expected-warning@-1:10 {{struct 'SafeCodable' has storage involving unsafe types}}{{documentation-file=strict-memory-safety}}
    // expected-note@-2:10 {{add '@unsafe' if this type is also unsafe to use}}{{3-3=@unsafe }}
    // expected-note@-3:10 {{add '@safe' if this type encapsulates the unsafe storage in a safe interface}}{{3-3=@safe }}
    var value: UnsafeCodable
    // expected-note@-1:9 {{property 'value' involves unsafe type 'Enabled.UnsafeCodable'}}
  }
}

// Nothing here is diagnosed, except for the always-unsafe use.
enum Disabled {
  struct Storage {
    var pointer: UnsafeMutablePointer<Int>
  }

  class Subclass: UnsafeBase { }

  class Override: SafeBase {
    @unsafe override func f() { }
  }

  struct Conformance: P {
    @unsafe func f() { }
  }

  static func expression() {
    unsafeFunc()
  }

  static func forIn() {
    for _ in UnsafeSequence() { }
  }

  static func mixed() {
    takesTwo(alwaysUnsafeValue(), unsafeValue())
    // expected-error@-1:5 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{5-5=unsafe }}{{documentation-file=always-unsafe}}
    // expected-note@-2:14 {{reference to unsafe global function 'alwaysUnsafeValue()'}}
  }
}

//--- file_default.swift

default @diagnose(StrictMemorySafety, as: warning)

@preconcurrency import unsafe_swift_decls
// expected-warning@-1:24 {{'@preconcurrency' import is not memory-safe because it can silently introduce data races; add '@unsafe' to indicate that this is unsafe}}{{1-1=@unsafe }}{{documentation-file=strict-memory-safety}}

@unsafe func unsafeFunc() { }

func expression() {
  unsafeFunc()
  // expected-warning@-1:3 {{expression uses unsafe constructs but is not marked with 'unsafe'}}{{3-3=unsafe }}{{documentation-file=strict-memory-safety}}
  // expected-note@-2:3 {{reference to unsafe global function 'unsafeFunc()'}}
}
