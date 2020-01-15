// Check that the diagnostics are produced regardless of what primary file we're using.
// RUN: %target-swift-frontend -typecheck -warnings-as-errors -primary-file %s %S/Inputs/inconsistent-implementation-only/2.swift %S/Inputs/inconsistent-implementation-only/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify
// RUN: %target-swift-frontend -typecheck -warnings-as-errors %s -primary-file %S/Inputs/inconsistent-implementation-only/2.swift %S/Inputs/inconsistent-implementation-only/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify
// RUN: %target-swift-frontend -typecheck -warnings-as-errors %s %S/Inputs/inconsistent-implementation-only/2.swift -primary-file %S/Inputs/inconsistent-implementation-only/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify
// RUN: %target-swift-frontend -typecheck -warnings-as-errors -primary-file %s -primary-file %S/Inputs/inconsistent-implementation-only/2.swift -primary-file %S/Inputs/inconsistent-implementation-only/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify
// RUN: %target-swift-frontend -typecheck -warnings-as-errors %s %S/Inputs/inconsistent-implementation-only/2.swift %S/Inputs/inconsistent-implementation-only/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify

@_implementationOnly import NotSoSecret // expected-no-note
import NotSoSecret2 // expected-no-warning
import NotSoSecret3 // expected-no-warning

@_implementationOnly import ActuallySecret // no-warning
import ActuallyOkay // no-warning

@_exported import Contradictory // expected-no-warning
