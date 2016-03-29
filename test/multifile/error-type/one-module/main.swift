// Try with and without whole module optimization

// RUN: %target-build-swift %S/library.swift %S/main.swift
// RUN: %target-build-swift -whole-module-optimization %S/library.swift %S/main.swift

// REQUIRES: executable_test

// REQUIRES: disabled // Fails on iPhone simulator target due to possible MC-JIT bug

extension NuclearMeltdown : ErrorProtocol {}
