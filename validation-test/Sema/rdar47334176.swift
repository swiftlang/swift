// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/rdar47334176_types.swiftmodule %S/Inputs/rdar47334176_types.swift
// RUN: %target-swift-frontend -I %t -typecheck %s

import rdar47334176_types

// To test all possibilities let's declare one of the types
// in the same module as function declaration which uses it.
struct S<V> : R {}

func foo<T : P, U>(_: T?, _: (T.V.V) -> Void) where T.V == E<U> {} // Ok
func bar<T : P, U>(_: T?, _: (T.V.V) -> Void) where T.V == S<U> {} // Ok
func baz<T : P, U>(_: T?, _: (T.V.V) -> Void) where T.V == C<U> {} // Ok
