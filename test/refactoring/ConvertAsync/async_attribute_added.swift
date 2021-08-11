// REQUIRES: concurrency

// RUN: %empty-directory(%t)

// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=SIMPLE %s
func simple(completion: @escaping (String) -> Void) { }
// SIMPLE: async_attribute_added.swift [[# @LINE-1]]:1 -> [[# @LINE-1]]:1
// SIMPLE-NEXT: @available(*, renamed: "simple()")
// SIMPLE-EMPTY:
// SIMPLE-NEXT: async_attribute_added.swift [[# @LINE-4]]:53 -> [[# @LINE-4]]:56
// SIMPLE-NEXT: {
// SIMPLE-NEXT: Task {
// SIMPLE-NEXT: let result = await simple()
// SIMPLE-NEXT: completion(result)
// SIMPLE-NEXT: }
// SIMPLE-NEXT: }
// SIMPLE-EMPTY:
// SIMPLE-NEXT: async_attribute_added.swift [[# @LINE-12]]:56 -> [[# @LINE-12]]:56
// SIMPLE-EMPTY:
// SIMPLE-EMPTY:
// SIMPLE-EMPTY:
// SIMPLE-NEXT: async_attribute_added.swift [[# @LINE-16]]:56 -> [[# @LINE-16]]:56
// SIMPLE-NEXT: func simple() async -> String { }

// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):5 | %FileCheck -check-prefix=OTHER-ARGS %s
func otherArgs(first: Int, second: String, completion: @escaping (String) -> Void) { }
// OTHER-ARGS: @available(*, renamed: "otherArgs(first:second:)")

// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):5 | %FileCheck -check-prefix=EMPTY-NAMES %s
func emptyNames(first: Int, _ second: String, completion: @escaping (String) -> Void) { }
// EMPTY-NAMES: @available(*, renamed: "emptyNames(first:_:)")
