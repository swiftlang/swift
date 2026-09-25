// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -module-name ShimModule -enable-testing -I %S/Inputs/shadowed-globals %S/Inputs/shadowed-globals/ShimModule.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -I %t -I %S/Inputs/shadowed-globals %s

// API notes give BaseSDK's macros a `Type:`, so they import as typed globals.
// ShimModule separately declares `internal` vars of the same names -- the shape
// a platform overlay is left in once the underlying macros gain types upstream
// and its hand-written wrappers become redundant. Importing ShimModule
// `@testable` exposes those internals alongside BaseSDK's globals, so every use
// of one of these names is ambiguous.

@testable import ShimModule
import BaseSDK

func takesTwo(_ a: DWORD, _ b: DWORD) {}

struct WinSDKConstants {
  // One ambiguous reference per expression is diagnosed correctly.
  func oneAmbiguousReferencePerExpression() {
    let flag1: DWORD = FILE_SHARE_READ // expected-error {{ambiguous use of 'FILE_SHARE_READ'}}
    let flag2: DWORD = FILE_SHARE_WRITE // expected-error {{ambiguous use of 'FILE_SHARE_WRITE'}}
    let flags: DWORD = flag1 | flag2
    _ = flags
  }

  // Still fine when the single ambiguous reference is part of a larger
  // expression, so it is the count that matters rather than the nesting.
  func oneAmbiguousReferenceInOperatorExpression() {
    let flags: DWORD = FILE_SHARE_READ | 2 // expected-error {{ambiguous use of 'FILE_SHARE_READ'}}
    _ = flags
  }

  // FIXME: Two or more ambiguous references in a single expression defeat the
  // diagnostic machinery, which bails out entirely instead of reporting either
  // one. Each of the following should report `ambiguous use of ...` the way the
  // cases above do.
  func twoAmbiguousReferencesViaOperator() {
    let flags: DWORD = FILE_SHARE_READ | FILE_SHARE_WRITE // expected-error {{failed to produce diagnostic for expression}}
    _ = flags
  }

  func twoAmbiguousReferencesAsArguments() {
    takesTwo(FILE_SHARE_READ, FILE_SHARE_WRITE) // expected-error {{failed to produce diagnostic for expression}}
  }

  func twoAmbiguousReferencesInTuple() {
    let flags: (DWORD, DWORD) = (FILE_SHARE_READ, FILE_SHARE_WRITE) // expected-error {{failed to produce diagnostic for expression}}
    _ = flags
  }

  // The two references do not even have to be to different declarations.
  func sameAmbiguousReferenceTwice() {
    let flags: DWORD = FILE_SHARE_READ | FILE_SHARE_READ // expected-error {{failed to produce diagnostic for expression}}
    _ = flags
  }
}
