// RUN: %empty-directory(%t)

enum E : Error { case e }

func anyCompletion(_ completion: (Any?, Error?) -> Void) {}

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
