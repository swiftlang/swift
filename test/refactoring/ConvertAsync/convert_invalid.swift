func callbackIntWithError(_ completion: (Int8, Error?) -> Void) {}

// rdar://79864182
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=INVALID-COND %s
callbackIntWithError { x, err in
  if x {
    print("ok")
  }
}
// INVALID-COND:      let x = try await callbackIntWithError()
// INVALID-COND-NEXT: if x {
// INVALID-COND-NEXT:   print("ok")
// INVALID-COND-NEXT: }

