// Try with and without whole module optimization, Swift 3 and Swift 4 mode

// RUN: %target-build-swift -swift-version 3 %S/Inputs/library.swift %S/main.swift
// RUN: %target-build-swift -swift-version 3 -whole-module-optimization %S/Inputs/library.swift %S/main.swift

// RUN: %target-build-swift -swift-version 4 %S/Inputs/library.swift %S/main.swift
// RUN: %target-build-swift -swift-version 4 -whole-module-optimization %S/Inputs/library.swift %S/main.swift

publicHasDefaultArgument()
internalHasDefaultArgument()
