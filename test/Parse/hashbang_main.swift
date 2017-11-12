#!/usr/bin/swift
let x = 42
x + x // expected-warning {{result of operator '+' is unused}}
// Check that we skip the hashbang at the beginning of the file.
// RUN: %target-typecheck-verify-swift

