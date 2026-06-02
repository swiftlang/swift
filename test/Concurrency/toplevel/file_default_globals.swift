// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 5 -module-name M %t/script_nonisolated.swift | %FileCheck --check-prefix=NONISO %s
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 5 -strict-concurrency=complete -module-name M %t/script_nonisolated.swift | %FileCheck --check-prefix=NONISO %s
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 6 -module-name M %t/script_nonisolated.swift | %FileCheck --check-prefix=NONISO %s
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 5 -module-name M %t/script_mainactor.swift | %FileCheck --check-prefix=MAIN %s
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 5 -strict-concurrency=complete -module-name M %t/script_mainactor.swift | %FileCheck --check-prefix=MAIN %s
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 6 -module-name M %t/script_mainactor.swift | %FileCheck --check-prefix=MAIN %s
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 5 -parse-as-library -module-name M %t/library_nonisolated.swift | %FileCheck --check-prefix=NONISO %s
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 5 -strict-concurrency=complete -parse-as-library -module-name M %t/library_nonisolated.swift | %FileCheck --check-prefix=NONISO %s
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 6 -parse-as-library -module-name M %t/library_nonisolated.swift | %FileCheck --check-prefix=NONISO %s
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 5 -parse-as-library -module-name M %t/library_mainactor.swift | %FileCheck --check-prefix=MAIN %s
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 5 -strict-concurrency=complete -parse-as-library -module-name M %t/library_mainactor.swift | %FileCheck --check-prefix=MAIN %s
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 6 -parse-as-library -module-name M %t/library_mainactor.swift | %FileCheck --check-prefix=MAIN %s
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -typecheck -swift-version 5 -verify %t/script_nonisolated_mutable.swift
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -typecheck -swift-version 5 -strict-concurrency=complete -verify -verify-additional-prefix complete- %t/script_nonisolated_mutable.swift
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -typecheck -swift-version 6 -verify -verify-additional-prefix swift6- %t/script_nonisolated_mutable.swift

// REQUIRES: concurrency
// REQUIRES: swift_feature_DefaultIsolationPerFile

//--- script_nonisolated.swift
using nonisolated

let storedLetGlobal = 0
var computedGlobal: Int { 0 }

//--- script_mainactor.swift
using @MainActor

let storedLetGlobal = 0
var storedVarGlobal = 0
var computedGlobal: Int { 0 }

//--- library_nonisolated.swift
using nonisolated

let storedLetGlobal = 0
var computedGlobal: Int { 0 }

//--- library_mainactor.swift
using @MainActor

let storedLetGlobal = 0
var storedVarGlobal = 0
var computedGlobal: Int { 0 }

//--- script_nonisolated_mutable.swift
using nonisolated

// A mutable nonisolated stored TLG is one error in any mode, but we complain more in later modes.
var storedVarGlobal = 0
// expected-complete-warning@-1 {{var 'storedVarGlobal' is not concurrency-safe because it is nonisolated global shared mutable state}}
// expected-complete-note@-2 {{convert 'storedVarGlobal' to a 'let' constant to make 'Sendable' shared state immutable}}
// expected-complete-note@-3 {{add '@MainActor' to make var 'storedVarGlobal' part of global actor 'MainActor'}}
// expected-complete-note@-4 {{disable concurrency-safety checks if accesses are protected by an external synchronization mechanism}}
// expected-swift6-error@-5 {{var 'storedVarGlobal' is not concurrency-safe because it is nonisolated global shared mutable state}}
// expected-swift6-note@-6 {{convert 'storedVarGlobal' to a 'let' constant to make 'Sendable' shared state immutable}}
// expected-swift6-note@-7 {{add '@MainActor' to make var 'storedVarGlobal' part of global actor 'MainActor'}}
// expected-swift6-note@-8 {{disable concurrency-safety checks if accesses are protected by an external synchronization mechanism}}
// expected-error@-9 {{'nonisolated' cannot be applied to mutable stored properties}}
// expected-note@-10 {{convert 'storedVarGlobal' to a 'let' constant or consider declaring it 'nonisolated(unsafe)' if manually managing concurrency safety}}

// MAIN: @MainActor {{.*}}let storedLetGlobal: Int
// MAIN: @MainActor {{.*}}var storedVarGlobal: Int
// MAIN: @MainActor {{.*}}var computedGlobal: Int

// NONISO: nonisolated let storedLetGlobal: Int
// NONISO: nonisolated var computedGlobal: Int
