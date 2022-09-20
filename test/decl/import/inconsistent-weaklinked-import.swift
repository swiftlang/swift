// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Check that the diagnostics are produced regardless of what primary file we're using.
// RUN: %target-swift-frontend -typecheck -primary-file %t/1.swift %t/2.swift %t/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify
// RUN: %target-swift-frontend -typecheck %t/1.swift -primary-file %t/2.swift %t/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify
// RUN: %target-swift-frontend -typecheck %t/1.swift %t/2.swift -primary-file %t/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify
// RUN: %target-swift-frontend -typecheck -primary-file %t/1.swift -primary-file %t/2.swift -primary-file %t/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify
// RUN: %target-swift-frontend -typecheck %t/1.swift %t/2.swift %t/3.swift -I %S/Inputs/inconsistent-implementation-only/ -verify

// UNSUPPORTED: OS=windows-msvc

//--- 1.swift

@_weakLinked import NotSoSecret // expected-note {{imported with @_weakLinked here}}
import NotSoSecret2 // expected-error {{'NotSoSecret2' inconsistently imported with @_weakLinked}} {{1-1=@_weakLinked }}
import NotSoSecret3 // expected-error {{'NotSoSecret3' inconsistently imported with @_weakLinked}} {{1-1=@_weakLinked }}

@_weakLinked import ActuallySecret // no-error
import ActuallyOkay // no-error

@_exported import Contradictory // expected-error {{'Contradictory' inconsistently imported with @_weakLinked}} {{12-12=@_weakLinked }}

//--- 2.swift

import NotSoSecret // expected-error {{'NotSoSecret' inconsistently imported with @_weakLinked}}
@_weakLinked import NotSoSecret2 // expected-note 2 {{imported with @_weakLinked here}}
import NotSoSecret3 // expected-error {{'NotSoSecret3' inconsistently imported with @_weakLinked}}

@_weakLinked import ActuallySecret // no-error
import ActuallyOkay // no-error

@_weakLinked import Contradictory // expected-note {{imported with @_weakLinked here}}

//--- 3.swift

@_weakLinked import NotSoSecret
import NotSoSecret2 // expected-error {{'NotSoSecret2' inconsistently imported with @_weakLinked}}
@_weakLinked import NotSoSecret3 // expected-note 2 {{imported with @_weakLinked here}}

@_weakLinked import ActuallySecret // no-error
import ActuallyOkay // no-error
