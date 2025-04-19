// RUN: %target-swift-frontend -typecheck -swift-version 7 -verify -verify-additional-prefix swift7- -verify-additional-prefix swift6+- %S/Inputs/accessors.swift

// REQUIRES: asserts
// REQUIRES: concurrency
