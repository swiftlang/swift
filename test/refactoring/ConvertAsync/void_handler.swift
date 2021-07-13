// REQUIRES: concurrency

// RUN: %empty-directory(%t)

// A (Void) parameter has a warning in Swift, so this function is pulled into
// its own file
// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix VOID-HANDLER %s
func voidCompletion(completion: @escaping (Void) -> Void) {}
// VOID-HANDLER: {
// VOID-HANDLER-NEXT: Task {
// VOID-HANDLER-NEXT: await voidCompletion()
// VOID-HANDLER-NEXT: completion(())
// VOID-HANDLER-NEXT: }
// VOID-HANDLER-NEXT: }
// VOID-HANDLER: func voidCompletion() async {}
