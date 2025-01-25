// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Check that the diagnostics are produced regardless of what primary file we're using.
// RUN: %target-swift-frontend -typecheck -primary-file %t/1.swift %t/2.swift %t/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -typecheck %t/1.swift -primary-file %t/2.swift %t/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -typecheck %t/1.swift %t/2.swift -primary-file %t/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -typecheck -primary-file %t/1.swift -primary-file %t/2.swift -primary-file %t/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -typecheck %t/1.swift %t/2.swift %t/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify -swift-version 5 -enable-library-evolution

//--- 1.swift

@_implementationOnly import NotSoSecret // expected-note {{imported as implementation-only here}}
import NotSoSecret2 // expected-warning {{'NotSoSecret2' inconsistently imported as implementation-only}} {{1-1=@_implementationOnly }}
import NotSoSecret3 // expected-warning {{'NotSoSecret3' inconsistently imported as implementation-only}} {{1-1=@_implementationOnly }}

@_implementationOnly import ActuallySecret // no-warning
import ActuallyOkay // no-warning

@_exported import Contradictory // expected-warning {{'Contradictory' inconsistently imported as implementation-only}} {{none}}

//--- 2.swift

import NotSoSecret // expected-warning {{'NotSoSecret' inconsistently imported as implementation-only}}
@_implementationOnly import NotSoSecret2 // expected-note 2 {{imported as implementation-only here}}
import NotSoSecret3 // expected-warning {{'NotSoSecret3' inconsistently imported as implementation-only}}

@_implementationOnly import ActuallySecret // no-warning
import ActuallyOkay // no-warning

@_implementationOnly import Contradictory // expected-note {{imported as implementation-only here}}

//--- 3.swift

@_implementationOnly import NotSoSecret
import NotSoSecret2 // expected-warning {{'NotSoSecret2' inconsistently imported as implementation-only}}
@_implementationOnly import NotSoSecret3 // expected-note 2 {{imported as implementation-only here}}

@_implementationOnly import ActuallySecret // no-warning
import ActuallyOkay // no-warning

