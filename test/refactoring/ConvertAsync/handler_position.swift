// REQUIRES: concurrency

// RUN: %empty-directory(%t)

// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+2):1 | %FileCheck -check-prefix=FIRST-ARG %s
// RUN: %refactor-check-compiles -add-async-wrapper -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=FIRST-ARG-WRAPPER %s
func completionFirst(/*c1s*/ completion: @escaping (String) -> Void /*c1e*/, /*c2s*/ arg: Int /*c2e*/) { }

// FIRST-ARG:      @available(*, renamed: "completionFirst(arg:)")
// FIRST-ARG:      Task {
// FIRST-ARG-NEXT:   let result = await completionFirst(arg: arg)
// FIRST-ARG-NEXT:   completion(result)
// FIRST-ARG-NEXT: }
// FIRST-ARG:      func completionFirst(/*c2s*/ arg: Int /*c2e*/) async -> String { }

// FIRST-ARG-WRAPPER:      @available(*, renamed: "completionFirst(arg:)")
// FIRST-ARG-WRAPPER:      func completionFirst(/*c2s*/ arg: Int /*c2e*/) async -> String {
// FIRST-ARG-WRAPPER-NEXT:   return await withCheckedContinuation { continuation in
// FIRST-ARG-WRAPPER-NEXT:     completionFirst(completion: { result in
// FIRST-ARG-WRAPPER-NEXT:       continuation.resume(returning: result)
// FIRST-ARG-WRAPPER-NEXT:     }, arg: arg)
// FIRST-ARG-WRAPPER-NEXT:   }
// FIRST-ARG-WRAPPER-NEXT: }

// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+2):1 | %FileCheck -check-prefix MIDDLE-ARG %s
// RUN: %refactor-check-compiles -add-async-wrapper -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=MIDDLE-ARG-WRAPPER %s
func completionMiddle(/*c1s*/ arg1: Int /*c1e*/, /*c2s*/ _ arg2: Int /*c2e*/, /*c3s*/ completion: @escaping (String) -> Void /*c3e*/, /*c4s*/ arg3: Int /*c4e*/) { }

// MIDDLE-ARG:      @available(*, renamed: "completionMiddle(arg1:_:arg3:)")
// MIDDLE-ARG:      Task {
// MIDDLE-ARG-NEXT:   let result = await completionMiddle(arg1: arg1, arg2, arg3: arg3)
// MIDDLE-ARG-NEXT:   completion(result)
// MIDDLE-ARG-NEXT: }
// MIDDLE-ARG:      func completionMiddle(/*c1s*/ arg1: Int /*c1e*/, /*c2s*/ _ arg2: Int /*c2e*/, /*c4s*/ arg3: Int /*c4e*/) async -> String { }

// MIDDLE-ARG-WRAPPER:      @available(*, renamed: "completionMiddle(arg1:_:arg3:)")
// MIDDLE-ARG-WRAPPER:      func completionMiddle(/*c1s*/ arg1: Int /*c1e*/, /*c2s*/ _ arg2: Int /*c2e*/, /*c4s*/ arg3: Int /*c4e*/) async -> String {
// MIDDLE-ARG-WRAPPER-NEXT:   return await withCheckedContinuation { continuation in
// MIDDLE-ARG-WRAPPER-NEXT:     completionMiddle(arg1: arg1, arg2, completion: { result in
// MIDDLE-ARG-WRAPPER-NEXT:       continuation.resume(returning: result)
// MIDDLE-ARG-WRAPPER-NEXT:     }, arg3: arg3)
// MIDDLE-ARG-WRAPPER-NEXT:   }
// MIDDLE-ARG-WRAPPER-NEXT: }
