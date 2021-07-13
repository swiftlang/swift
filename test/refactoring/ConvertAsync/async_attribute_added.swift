// REQUIRES: concurrency

// RUN: %empty-directory(%t)

// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 -enable-experimental-concurrency | %FileCheck -check-prefix=SIMPLE %s
func simple(completion: @escaping (String) -> Void) { }
// SIMPLE: async_attribute_added.swift [[# @LINE-1]]:1 -> [[# @LINE-1]]:1
// SIMPLE-NEXT: @available(*, deprecated, message: "Prefer async alternative instead")
// SIMPLE-EMPTY:
// SIMPLE-NEXT: async_attribute_added.swift [[# @LINE-4]]:1 -> [[# @LINE-4]]:1
// SIMPLE-NEXT: @completionHandlerAsync("simple()", completionHandlerIndex: 0)
// SIMPLE-EMPTY:
// SIMPLE-NEXT: async_attribute_added.swift [[# @LINE-7]]:53 -> [[# @LINE-7]]:56
// SIMPLE-NEXT: {
// SIMPLE-NEXT: Task {
// SIMPLE-NEXT: let result = await simple()
// SIMPLE-NEXT: completion(result)
// SIMPLE-NEXT: }
// SIMPLE-NEXT: }
// SIMPLE-EMPTY:
// SIMPLE-NEXT: async_attribute_added.swift [[# @LINE-15]]:56 -> [[# @LINE-15]]:56
// SIMPLE-EMPTY:
// SIMPLE-EMPTY:
// SIMPLE-EMPTY:
// SIMPLE-NEXT: async_attribute_added.swift [[# @LINE-19]]:56 -> [[# @LINE-19]]:56
// SIMPLE-NEXT: func simple() async -> String { }

// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):5 -enable-experimental-concurrency | %FileCheck -check-prefix=OTHER-ARGS %s
func otherArgs(first: Int, second: String, completion: @escaping (String) -> Void) { }
// OTHER-ARGS: @completionHandlerAsync("otherArgs(first:second:)", completionHandlerIndex: 2)

// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):5 -enable-experimental-concurrency | %FileCheck -check-prefix=EMPTY-NAMES %s
func emptyNames(first: Int, _ second: String, completion: @escaping (String) -> Void) { }
// EMPTY-NAMES: @completionHandlerAsync("emptyNames(first:_:)", completionHandlerIndex: 2)

// Not a completion handler named parameter, but should still be converted
// during function conversion since it has been attributed
@completionHandlerAsync("otherName()", completionHandlerIndex: 0)
func otherName(notHandlerName: @escaping (String) -> (Void)) {}
func otherName() async -> String {}

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):5 -enable-experimental-concurrency | %FileCheck -check-prefix=OTHER-CONVERTED %s
func otherStillConverted() {
  otherName { str in
    print(str)
  }
}
// OTHER-CONVERTED: func otherStillConverted() async {
// OTHER-CONVERTED-NEXT: let str = await otherName()
// OTHER-CONVERTED-NEXT: print(str)
