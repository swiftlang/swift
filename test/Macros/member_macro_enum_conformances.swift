//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -module-name MacroDefinition %S/Inputs/syntax_macro_definitions.swift -o %t/%target-library-name(MacroDefinition) -no-toolchain-stdlib-rpath
// RUN: %target-swift-frontend -swift-version 5 -emit-module -module-name EnumConformances -load-plugin-library %t/%target-library-name(MacroDefinition) %t/Enums.swift %t/Uses.swift -o %t/EnumConformances.swiftmodule
// RUN: %target-swift-frontend -swift-version 5 -emit-module -module-name EnumConformances -load-plugin-library %t/%target-library-name(MacroDefinition) %t/Uses.swift %t/Enums.swift -o %t/EnumConformances.swiftmodule
// RUN: %target-swift-frontend -swift-version 6 -emit-module -module-name EnumConformances -load-plugin-library %t/%target-library-name(MacroDefinition) %t/Uses.swift %t/Enums.swift -o %t/EnumConformances.swiftmodule
// RUN: %target-swift-frontend -swift-version 6 -emit-module -module-name EnumConformances -load-plugin-library %t/%target-library-name(MacroDefinition) -primary-file %t/Uses.swift %t/Enums.swift -o %t/EnumConformances-primary.swiftmodule
// RUN: %target-swift-frontend -swift-version 5 -typecheck -verify -DINVALID -load-plugin-library %t/%target-library-name(MacroDefinition) -primary-file %t/Uses.swift %t/Enums.swift
// RUN: %target-swift-frontend -swift-version 6 -typecheck -verify -DINVALID -load-plugin-library %t/%target-library-name(MacroDefinition) -primary-file %t/Uses.swift %t/Enums.swift

// Conformance lookup from another file must see the expanded cases. In
// particular, Uses.swift must not name a generated case: that name lookup
// would expand the macro and hide premature implicit-conformance synthesis.

//--- Enums.swift
@attached(member, names: named(generated))
macro AddCase(_ code: String) = #externalMacro(module: "MacroDefinition", type: "MemberThatEmitsCodeMacro")

// These enums gain implicit Equatable/Hashable conformances only after expansion.
@AddCase("case generated")
enum GeneratedOnly {}

@AddCase("case generated")
enum RawGenerated: String {}

// These enums must not gain implicit Equatable/Hashable conformances, whether
// or not the generated payload itself conforms to those protocols.
@AddCase("case generated(T)")
enum GenericPayload<T> {
  case source
}

@AddCase("case generated(Int)")
enum IntPayload {
  case source
}

// Explicit conformance can still be synthesized from the generated payload.
@AddCase("case generated(T)")
enum ExplicitPayload<T: Hashable>: Hashable {
  case source
}

struct EquatableOnly: Equatable {
  var value: Int
}

@AddCase("case generated(T)")
enum ExplicitEquatablePayload<T: Equatable>: Equatable {
  case source
}

//--- Uses.swift
func needsEquatable<T: Equatable>(_: T.Type) {} // expected-note 2 {{where 'T' = }}
func needsHashable<T: Hashable>(_: T.Type) {} // expected-note 3 {{where 'T' = }}
func needsRawRepresentable<T: RawRepresentable>(_: T.Type) {}

func checkConformances() {
  needsEquatable(GeneratedOnly.self)
  needsHashable(GeneratedOnly.self)
  needsEquatable(RawGenerated.self)
  needsHashable(RawGenerated.self)
  needsRawRepresentable(RawGenerated.self)
  needsEquatable(ExplicitPayload<Int>.self)
  needsHashable(ExplicitPayload<Int>.self)
  needsEquatable(ExplicitEquatablePayload<EquatableOnly>.self)
}

func echo<T>(_ value: GenericPayload<T>) -> GenericPayload<T> { value }

#if INVALID
func checkPayloadConformances() {
  needsEquatable(GenericPayload<Int>.self) // expected-error {{global function 'needsEquatable' requires that 'GenericPayload<Int>' conform to 'Equatable'}}
  needsHashable(GenericPayload<Int>.self) // expected-error {{global function 'needsHashable' requires that 'GenericPayload<Int>' conform to 'Hashable'}}
  needsEquatable(IntPayload.self) // expected-error {{global function 'needsEquatable' requires that 'IntPayload' conform to 'Equatable'}}
  needsHashable(IntPayload.self) // expected-error {{global function 'needsHashable' requires that 'IntPayload' conform to 'Hashable'}}
  needsHashable(ExplicitEquatablePayload<Int>.self) // expected-error {{global function 'needsHashable' requires that 'ExplicitEquatablePayload<Int>' conform to 'Hashable'}}
}
#endif
