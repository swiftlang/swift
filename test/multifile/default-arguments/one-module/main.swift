// Try with and without whole module optimization

// RUN: %target-build-swift -swift-version 4 %S/Inputs/library.swift %S/main.swift
// RUN: %target-build-swift -swift-version 4 -whole-module-optimization %S/Inputs/library.swift %S/main.swift

publicHasDefaultArgument()
internalHasDefaultArgument()
