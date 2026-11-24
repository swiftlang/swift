// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// A stdlib built from source serializes the invertible protocols that were
// synthesized into it. A module built with -parse-stdlib that doesn't import
// the stdlib refers to those protocols in the Builtin module instead. Both
// references must resolve to the same protocol declarations in a client that
// imports both modules.

// RUN: %target-swift-frontend -emit-module -parse-stdlib -module-name Swift \
// RUN:   -o %t/Swift.swiftmodule %t/Swift.swift
// RUN: %target-swift-frontend -emit-module -parse-stdlib -module-name Lib \
// RUN:   -o %t/Lib.swiftmodule %t/Lib.swift

// RUN: %target-swift-frontend -typecheck -verify -parse-stdlib -I %t \
// RUN:   %t/ClientImportingSwiftFirst.swift
// RUN: %target-swift-frontend -typecheck -verify -parse-stdlib -I %t \
// RUN:   %t/ClientImportingLibFirst.swift

//--- Swift.swift
public struct Int {}

//--- Lib.swift
public struct S {}

//--- ClientImportingSwiftFirst.swift
import Swift
import Lib

func takeInt(_ x: Int) {}
func takeS(_ x: S) {} // Requires that S is Copyable.

//--- ClientImportingLibFirst.swift
import Lib
import Swift

func takeInt(_ x: Int) {}
func takeS(_ x: S) {} // Requires that S is Copyable.
