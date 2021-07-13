// REQUIRES: concurrency

// RUN: %empty-directory(%t)

@available(*, renamed: "simple2")
func simple(_ completion: @escaping (String) -> Void) { }
@available(*, renamed: "simple2")
func nonCompletionName(_ random: @escaping (String) -> Void) { }
func simple2() async -> String { }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):6 | %FileCheck -check-prefix=SIMPLERENAMED %s
func simpleRenamed() {
  // preserve me
  simple { str in
    print(str)
  }
  // and me
  nonCompletionName { str in
    print(str)
  }
}
// SIMPLERENAMED: func simpleRenamed() async {
// SIMPLERENAMED-NEXT: // preserve me
// SIMPLERENAMED-NEXT: let str = await simple2()
// SIMPLERENAMED-NEXT: print(str)
// SIMPLERENAMED-NEXT: // and me
// SIMPLERENAMED-NEXT: let str1 = await simple2()
// SIMPLERENAMED-NEXT: print(str1)

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):6 | %FileCheck -check-prefix=RENAMEDCOLLISION %s
func renamedCollision() {
  simple { simple2 in
    print(simple2)
  }
}
// RENAMEDCOLLISION: func renamedCollision() async {
// RENAMEDCOLLISION-NEXT: let simple21 = await simple2()
// RENAMEDCOLLISION-NEXT: print(simple21)

@available(*, renamed: "simpleArg2")
func simpleArgRenamed(arg: String, _ random: @escaping (String) -> Void) { }
@available(*, renamed: "simpleArg2")
func completionFirstArg(random: @escaping (String) -> Void, arg: String) { }
func simpleArg2(newArg: String) async -> String { }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):6 | %FileCheck -check-prefix=ARGRENAMED %s
func argRenamed() {
  (simpleArgRenamed)(arg: "foo") { str in
    print(str)
  }
  (completionFirstArg)(random: { str in
    print(str)
  }, arg: "foo")
}
// ARGRENAMED: func argRenamed() async {
// ARGRENAMED-NEXT: let str = await (simpleArg2)(newArg: "foo")
// ARGRENAMED-NEXT: print(str)
// ARGRENAMED-NEXT: let str1 = await (simpleArg2)(newArg: "foo")
// ARGRENAMED-NEXT: print(str1)

@available(*, renamed: "multiHandlers2")
func multiHandlers(arg: String, handler1: @escaping (String) -> Void, handler2: @escaping (String) -> Void) { }
func multiHandlers2(newArg: String, newHandler: @escaping (String) -> Void) async -> String { }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):6 | %FileCheck -check-prefix=MULTIPLEHANDLERS %s
func multipleHandlers() {
  multiHandlers(arg: "foo", handler1: { str1 in
    print(str1)
  }, handler2: { str2 in
    print(str2)
  })
  multiHandlers(arg: "foo", handler1: { str3 in
    print(str3)
  }) { str4 in
    print(str4)
  }
  multiHandlers(arg: "foo") { str5 in
    print(str5)
  } handler2: { str6 in
    print(str6)
  }
}
// MULTIPLEHANDLERS: func multipleHandlers() async {
// MULTIPLEHANDLERS-NEXT: let str2 = await multiHandlers2(newArg: "foo", newHandler: { str1 in
// MULTIPLEHANDLERS-NEXT: print(str1)
// MULTIPLEHANDLERS-NEXT: })
// MULTIPLEHANDLERS-NEXT: print(str2)
// MULTIPLEHANDLERS-NEXT: let str4 = await multiHandlers2(newArg: "foo", newHandler: { str3 in
// MULTIPLEHANDLERS-NEXT: print(str3)
// MULTIPLEHANDLERS-NEXT: })
// MULTIPLEHANDLERS-NEXT: print(str4)
// MULTIPLEHANDLERS-NEXT: let str6 = await multiHandlers2(newArg: "foo", newHandler: { str5 in
// MULTIPLEHANDLERS-NEXT: print(str5)
// MULTIPLEHANDLERS-NEXT: })
// MULTIPLEHANDLERS-NEXT: print(str6)
