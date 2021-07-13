// RUN: %empty-directory(%t)

func simpleWithError(completion: (String?, Error?) -> Void) {}
func simpleWithError() async throws -> String {}

func testPathClassification() async throws {

  // Both of these test cases should produce the same refactoring.

  // RUN: %refactor-check-compiles -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=ELSE-IF-CLASSIFICATION %s
  simpleWithError { str, err in
    if err == nil {
      print("a")
    } else if .random() {
      print("b")
    } else {
      print("c")
    }
    if err != nil {
      print("d")
    } else if .random() {
      print("e")
    } else {
      print("f")
    }
  }

  // RUN: %refactor-check-compiles -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=ELSE-IF-CLASSIFICATION %s
  simpleWithError { str, err in
    if let str = str {
      print("a")
    } else if .random() {
      print("b")
    } else {
      print("c")
    }
    if str == nil {
      print("d")
    } else if .random() {
      print("e")
    } else {
      print("f")
    }
  }

  // ELSE-IF-CLASSIFICATION:      do {
  // ELSE-IF-CLASSIFICATION-NEXT:   let str = try await simpleWithError()
  // ELSE-IF-CLASSIFICATION-NEXT:   print("a")
  // ELSE-IF-CLASSIFICATION-NEXT:   if .random() {
  // ELSE-IF-CLASSIFICATION-NEXT:     print("e")
  // ELSE-IF-CLASSIFICATION-NEXT:   } else {
  // ELSE-IF-CLASSIFICATION-NEXT:     print("f")
  // ELSE-IF-CLASSIFICATION-NEXT:   }
  // ELSE-IF-CLASSIFICATION-NEXT: } catch let err {
  // ELSE-IF-CLASSIFICATION-NEXT:   if .random() {
  // ELSE-IF-CLASSIFICATION-NEXT:     print("b")
  // ELSE-IF-CLASSIFICATION-NEXT:   } else {
  // ELSE-IF-CLASSIFICATION-NEXT:     print("c")
  // ELSE-IF-CLASSIFICATION-NEXT:   }
  // ELSE-IF-CLASSIFICATION-NEXT:   print("d")
  // ELSE-IF-CLASSIFICATION-NEXT: }

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=ELSE-IF-CLASSIFICATION2 %s
  simpleWithError { str, err in
    if err == nil {
      print("a")
    } else if .random() {
      print("b")
    }
    if .random() {
      print("c")
    }
    if err != nil, .random() {
      print("d")
    }
  }

  // Make sure the classification of 'b' into the error block doesn't affect the
  // handling of 'c'.
  // ELSE-IF-CLASSIFICATION2:      do {
  // ELSE-IF-CLASSIFICATION2-NEXT:   let str = try await simpleWithError()
  // ELSE-IF-CLASSIFICATION2-NEXT:   print("a")
  // ELSE-IF-CLASSIFICATION2-NEXT:   if .random() {
  // ELSE-IF-CLASSIFICATION2-NEXT:     print("c")
  // ELSE-IF-CLASSIFICATION2-NEXT:   }
  // ELSE-IF-CLASSIFICATION2-NEXT: } catch let err {
  // ELSE-IF-CLASSIFICATION2-NEXT:   if .random() {
  // ELSE-IF-CLASSIFICATION2-NEXT:     print("b")
  // ELSE-IF-CLASSIFICATION2-NEXT:   }
  // ELSE-IF-CLASSIFICATION2-NEXT:   if <#err#> != nil, .random() {
  // ELSE-IF-CLASSIFICATION2-NEXT:     print("d")
  // ELSE-IF-CLASSIFICATION2-NEXT:   }
  // ELSE-IF-CLASSIFICATION2-NEXT: }

  // FIXME: This case is a little tricky: Being in the else block of 'err == nil'
  // should allow us to place 'if let str = str' in the failure block. However, this
  // is a success condition, so we still place it in the success block. Really what
  // we need to do here is check to see if simpleWithError has an existing async
  // alternative that still returns optional success values, and allow success
  // classification in that case. Otherwise, we'd probably be better off leaving
  // the condition unhandled, as it's not clear what the user is doing.
  // RUN: %refactor-check-compiles -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=ELSE-IF-CLASSIFICATION3 %s
  simpleWithError { str, err in
    if err == nil {
      print("a")
    } else if let str = str {
      print("b")
    } else {
      print("c")
    }
  }

  // ELSE-IF-CLASSIFICATION3:      do {
  // ELSE-IF-CLASSIFICATION3-NEXT:   let str = try await simpleWithError()
  // ELSE-IF-CLASSIFICATION3-NEXT:   print("a")
  // ELSE-IF-CLASSIFICATION3-NEXT:   print("b")
  // ELSE-IF-CLASSIFICATION3-NEXT: } catch let err {
  // ELSE-IF-CLASSIFICATION3-NEXT:   print("c")
  // ELSE-IF-CLASSIFICATION3-NEXT: }

  // FIXME: Similar to the case above.
  // RUN: %refactor-check-compiles -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=ELSE-IF-CLASSIFICATION4 %s
  simpleWithError { str, err in
    if err == nil {
      print("a")
    } else if let str = str {
      print("b")
    } else if .random() {
      print("c")
    } else {
      print("d")
    }
  }

  // ELSE-IF-CLASSIFICATION4:      do {
  // ELSE-IF-CLASSIFICATION4-NEXT:   let str = try await simpleWithError()
  // ELSE-IF-CLASSIFICATION4-NEXT:   print("a")
  // ELSE-IF-CLASSIFICATION4-NEXT:   print("b")
  // ELSE-IF-CLASSIFICATION4-NEXT: } catch let err {
  // ELSE-IF-CLASSIFICATION4-NEXT:   if .random() {
  // ELSE-IF-CLASSIFICATION4-NEXT:     print("c")
  // ELSE-IF-CLASSIFICATION4-NEXT:   } else {
  // ELSE-IF-CLASSIFICATION4-NEXT:     print("d")
  // ELSE-IF-CLASSIFICATION4-NEXT:   }
  // ELSE-IF-CLASSIFICATION4-NEXT: }

  // RUN: %refactor-check-compiles -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=ELSE-IF-CLASSIFICATION5 %s
  simpleWithError { str, err in
    if let err = err {
      print("a")
    } else if let str = str {
      print("b")
      return
    } else if .random() {
      print("c")
    } else {
      print("d")
    }
  }

  // ELSE-IF-CLASSIFICATION5:      do {
  // ELSE-IF-CLASSIFICATION5-NEXT:   let str = try await simpleWithError()
  // ELSE-IF-CLASSIFICATION5-NEXT:   print("b")
  // ELSE-IF-CLASSIFICATION5-NEXT: } catch let err {
  // ELSE-IF-CLASSIFICATION5-NEXT:   print("a")
  // ELSE-IF-CLASSIFICATION5-NEXT:   if .random() {
  // ELSE-IF-CLASSIFICATION5-NEXT:     print("c")
  // ELSE-IF-CLASSIFICATION5-NEXT:   } else {
  // ELSE-IF-CLASSIFICATION5-NEXT:     print("d")
  // ELSE-IF-CLASSIFICATION5-NEXT:   }
  // ELSE-IF-CLASSIFICATION5-NEXT: }

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=IF-LET-RETURN-CLASSIFICATION %s
  simpleWithError { str, err in
    if let str = str {
      print("a")
      return
    }
    if .random(), let err = err {
      print("b")
    } else {
      print("c")
    }
  }

  // IF-LET-RETURN-CLASSIFICATION:      do {
  // IF-LET-RETURN-CLASSIFICATION-NEXT:   let str = try await simpleWithError()
  // IF-LET-RETURN-CLASSIFICATION-NEXT:   print("a")
  // IF-LET-RETURN-CLASSIFICATION-NEXT: } catch let err {
  // IF-LET-RETURN-CLASSIFICATION-NEXT:   if .random(), let err = <#err#> {
  // IF-LET-RETURN-CLASSIFICATION-NEXT:     print("b")
  // IF-LET-RETURN-CLASSIFICATION-NEXT:   } else {
  // IF-LET-RETURN-CLASSIFICATION-NEXT:     print("c")
  // IF-LET-RETURN-CLASSIFICATION-NEXT:   }
  // IF-LET-RETURN-CLASSIFICATION-NEXT: }

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=GUARD-CLASSIFICATION %s
  simpleWithError { str, err in
    guard let str = str else {
      print("a")
      return
    }
    guard err == nil, .random() else {
      print("b")
      return
    }
    print("c")
  }

  // GUARD-CLASSIFICATION:      do {
  // GUARD-CLASSIFICATION-NEXT:   let str = try await simpleWithError()
  // GUARD-CLASSIFICATION-NEXT:   guard <#err#> == nil, .random() else {
  // GUARD-CLASSIFICATION-NEXT:     print("b")
  // GUARD-CLASSIFICATION-NEXT:     <#return#>
  // GUARD-CLASSIFICATION-NEXT:   }
  // GUARD-CLASSIFICATION-NEXT:   print("c")
  // GUARD-CLASSIFICATION-NEXT: } catch let err {
  // GUARD-CLASSIFICATION-NEXT:   print("a")
  // GUARD-CLASSIFICATION-NEXT: }

  // RUN: %refactor-check-compiles -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=SILLY-CLASSIFICATION %s
  simpleWithError { str, err in
    guard let str = str else { return }
    guard let err = err else { return }
    print("urr")
  }

  // In this case we just take whichever guard is last.
  // SILLY-CLASSIFICATION:      do {
  // SILLY-CLASSIFICATION-NEXT:   let str = try await simpleWithError()
  // SILLY-CLASSIFICATION-NEXT: } catch let err {
  // SILLY-CLASSIFICATION-NEXT:   print("urr")
  // SILLY-CLASSIFICATION-NEXT: }
}
