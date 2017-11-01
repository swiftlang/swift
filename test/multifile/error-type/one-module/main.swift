// Try with and without whole module optimization

// RUN: %target-build-swift %S/Inputs/library.swift %S/main.swift
// RUN: %target-build-swift -whole-module-optimization %S/Inputs/library.swift %S/main.swift

extension NuclearMeltdown : Error {}
