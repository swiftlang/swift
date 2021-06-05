// RUN: %empty-directory(%t)

enum E : Error { case e }

func anyCompletion(_ completion: (Any?, Error?) -> Void) {}
func anyResultCompletion(_ completion: (Result<Any?, Error>) -> Void) {}

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=TEST-UNHANDLED %s
anyCompletion { val, err in
  if let x = val {
    print("a")
  }
  if let _ = val {
    print("b")
  }
  if case let x? = val {
    print("c")
  }
  if case let _? = val {
    print("d")
  }
  if case E.e? = val {
    print("e")
  }
  if case (let x as String)? = val {
    print("f")
  }
  if case let "" as String = val {
    print("g")
  }
}

// TEST-UNHANDLED:      let x = try await anyCompletion()
// TEST-UNHANDLED-NEXT: print("a")
// TEST-UNHANDLED-NEXT: print("b")
// TEST-UNHANDLED-NEXT: print("c")
// TEST-UNHANDLED-NEXT: print("d")
// TEST-UNHANDLED-NEXT: if case E.e? = <#x#> {
// TEST-UNHANDLED-NEXT:   print("e")
// TEST-UNHANDLED-NEXT: }
// TEST-UNHANDLED-NEXT: if case (let x as String)? = <#x#> {
// TEST-UNHANDLED-NEXT:   print("f")
// TEST-UNHANDLED-NEXT: }
// TEST-UNHANDLED-NEXT: if case let "" as String = <#x#> {
// TEST-UNHANDLED-NEXT:   print("g")
// TEST-UNHANDLED-NEXT: }

// RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
anyResultCompletion { res in
  switch res {
  case .success(let unwrapped?):
    print(unwrapped)
  case .success(let x):
    print(x)
  case .failure:
    print("oh no")
  }
}

// RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
anyResultCompletion { res in
  switch res {
  case .success(let str as String):
    print(str)
  case .success(let x):
    print(x)
  case .failure:
    print("oh no")
  }
}

// RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
anyResultCompletion { res in
  switch res {
  case .success(E.e?):
    print("ee")
  case .success(let x):
    print(x)
  case .failure:
    print("oh no")
  }
}

// RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
anyResultCompletion { res in
  switch res {
  case .success("" as String):
    print("empty string")
  case .success(let x):
    print(x)
  case .failure:
    print("oh no")
  }
}

// FIXME: This should ideally be turned into a 'catch let e as E'.
// RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
anyResultCompletion { res in
  switch res {
  case .success(let a):
    print(a)
  case .failure(let e as E):
    print("oh no e")
  case .failure:
    print("oh no")
  }
}

// FIXME: This should ideally be turned into a 'catch E.e'.
// RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
anyResultCompletion { res in
  switch res {
  case .success(let a):
    print(a)
  case .failure(E.e):
    print("oh no ee")
  case .failure:
    print("oh no")
  }
}
