// RUN: %target-swift-frontend -typecheck -parse-as-library -swift-version 7 -verify -verify-additional-prefix swift7- -verify-additional-prefix swift6+- %S/Inputs/accessors_global_var.swift

// REQUIRES: asserts
// REQUIRES: concurrency
