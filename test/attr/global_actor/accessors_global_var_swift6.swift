// RUN: %target-swift-frontend -typecheck -parse-as-library -swift-version 6 -verify -verify-additional-prefix swift6- -verify-additional-prefix swift6+- %S/Inputs/accessors_global_var.swift

// REQUIRES: concurrency
