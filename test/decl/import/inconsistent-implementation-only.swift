// Check that the diagnostics are produced regardless of what primary file we're using.
// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/inconsistent-implementation-only/2.swift %S/Inputs/inconsistent-implementation-only/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify
// RUN: %target-swift-frontend -typecheck %s -primary-file %S/Inputs/inconsistent-implementation-only/2.swift %S/Inputs/inconsistent-implementation-only/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify
// RUN: %target-swift-frontend -typecheck %s %S/Inputs/inconsistent-implementation-only/2.swift -primary-file %S/Inputs/inconsistent-implementation-only/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify
// RUN: %target-swift-frontend -typecheck -primary-file %s -primary-file %S/Inputs/inconsistent-implementation-only/2.swift -primary-file %S/Inputs/inconsistent-implementation-only/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify
// RUN: %target-swift-frontend -typecheck %s %S/Inputs/inconsistent-implementation-only/2.swift %S/Inputs/inconsistent-implementation-only/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify

@_implementationOnly import NotSoSecret // expected-note {{imported as implementation-only here}}
import NotSoSecret2 // expected-warning {{'NotSoSecret2' inconsistently imported as implementation-only}} {{1-1=@_implementationOnly }}
import NotSoSecret3 // expected-warning {{'NotSoSecret3' inconsistently imported as implementation-only}} {{1-1=@_implementationOnly }}

@_implementationOnly import ActuallySecret // no-warning
import ActuallyOkay // no-warning

@_exported import Contradictory // expected-warning {{'Contradictory' inconsistently imported as implementation-only}} {{none}}
