// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// A diagnostic that is downgraded to a warning until a future language mode
// says that it will become an error. When the warning is upgraded back to an
// error, that suffix is wrong and should be dropped.

// "...in a future Swift language mode" wrapper (no diagnostic group).
// RUN: %target-swift-frontend -typecheck -diagnostic-style llvm %t/future.swift 2>&1 | %FileCheck %s --check-prefix=FUTURE-WARN
// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm -warnings-as-errors %t/future.swift 2>&1 | %FileCheck %s --check-prefix=FUTURE-ERR
// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm -warnings-as-errors -debug-diagnostic-names %t/future.swift 2>&1 | %FileCheck %s --check-prefix=FUTURE-NAMES

// FUTURE-WARN: warning: attribute '@Sendable' cannot be applied to a type alias; this will be an error in a future Swift language mode{{$}}

// FUTURE-ERR: error: attribute '@Sendable' cannot be applied to a type alias{{$}}
// FUTURE-ERR-NOT: will be an error

// FUTURE-NAMES: error: attribute '@Sendable' cannot be applied to a type alias [attribute_part_of_type]{{$}}

// "...in the Swift N language mode" wrapper, in group MutableGlobalVariable.
// RUN: %target-swift-frontend -typecheck -diagnostic-style llvm -parse-as-library -swift-version 5 -strict-concurrency=complete -print-diagnostic-groups %t/swift6.swift 2>&1 | %FileCheck %s --check-prefix=SWIFT6-WARN
// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm -parse-as-library -swift-version 5 -strict-concurrency=complete -print-diagnostic-groups -warnings-as-errors %t/swift6.swift 2>&1 | %FileCheck %s --check-prefix=SWIFT6-ERR
// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm -parse-as-library -swift-version 5 -strict-concurrency=complete -print-diagnostic-groups -Werror MutableGlobalVariable %t/swift6.swift 2>&1 | %FileCheck %s --check-prefix=SWIFT6-ERR
// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm -parse-as-library -swift-version 5 -strict-concurrency=complete -debug-diagnostic-names -Werror MutableGlobalVariable %t/swift6.swift 2>&1 | %FileCheck %s --check-prefix=SWIFT6-NAMES

// SWIFT6-WARN: warning: var 'globalCounter' is not concurrency-safe because it is nonisolated global shared mutable state; this is an error in the Swift 6 language mode [#MutableGlobalVariable]{{$}}

// SWIFT6-ERR: error: var 'globalCounter' is not concurrency-safe because it is nonisolated global shared mutable state [#MutableGlobalVariable]{{$}}
// SWIFT6-ERR-NOT: is an error in the Swift

// SWIFT6-NAMES: error: var 'globalCounter' is not concurrency-safe because it is nonisolated global shared mutable state [shared_mutable_state_decl] [#MutableGlobalVariable]{{$}}

//--- future.swift
typealias F = () -> ()
func foo(_ f: @escaping @Sendable F) {}

//--- swift6.swift
var globalCounter = 0
