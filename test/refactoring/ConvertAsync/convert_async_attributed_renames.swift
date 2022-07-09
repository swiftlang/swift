// REQUIRES: concurrency

// RUN: %empty-directory(%t)

@available(*, renamed: "simple2")
func simple(_ completion: @escaping (String) -> Void) { }
@available(*, renamed: "simple2")
func nonCompletionName(_ random: @escaping (String) -> Void) { }
func simple2() async -> String { return "" }

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
// SIMPLERENAMED-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):6 | %FileCheck -check-prefix=RENAMEDCOLLISION %s
func renamedCollision() {
  simple { simple2 in
    print(simple2)
  }
}
// RENAMEDCOLLISION: func renamedCollision() async {
// RENAMEDCOLLISION-NEXT: let simple21 = await simple2()
// RENAMEDCOLLISION-NEXT: print(simple21)
// RENAMEDCOLLISION-NEXT: }

@available(*, renamed: "simpleArg2")
func simpleArgRenamed(arg: String, _ random: @escaping (String) -> Void) { }
@available(*, renamed: "simpleArg2")
func completionFirstArg(random: @escaping (String) -> Void, arg: String) { }
func simpleArg2(newArg: String) async -> String { return "" }

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
// ARGRENAMED-NEXT: }

@available(*, renamed: "multiHandlers2")
func multiHandlers(arg: String, handler1: @escaping (String) -> Void, handler2: @escaping (String) -> Void) { }
func multiHandlers2(newArg: String, newHandler: @escaping (String) -> Void) async -> String { return "" }

@available(*, renamed: "multiHandlersWithTrailing2")
func multiHandlersWithTrailing(arg: String, handler1: @escaping (String) -> Void, handler2: @escaping (String) -> Void, other: String) { }
func multiHandlersWithTrailing2(newArg: String, newHandler: @escaping (String) -> Void, other: String) async -> String { return "" }

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
  multiHandlersWithTrailing(arg: "foo", handler1: { str7 in
    print(str7)
  }, handler2: { str8 in
    print(str8)
  }, other: "bar")
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
// MULTIPLEHANDLERS-NEXT: let str8 = await multiHandlersWithTrailing2(newArg: "foo", newHandler: { str7 in
// MULTIPLEHANDLERS-NEXT: print(str7)
// MULTIPLEHANDLERS-NEXT: }, other: "bar")
// MULTIPLEHANDLERS-NEXT: print(str8)
// MULTIPLEHANDLERS-NEXT: }

@available(*, renamed: "defaultedParamsStartNew(newArg:newArg1:)")
func defaultedParamsStart(arg1: Int, completionHandler: @escaping (String) -> Void) { }
func defaultedParamsStartNew(newArg: Int = 0, newArg1: Int) async -> String { return "" }

@available(*, renamed: "defaultedParamsMiddleNew(newArg1:newArg:newArg2:)")
func defaultedParamsMiddle(arg1: Int, arg2: Int, completionHandler: @escaping (String) -> Void) { }
func defaultedParamsMiddleNew(newArg1: Int, newArg: Int = 0, newArg2: Int) async -> String { return "" }

@available(*, renamed: "defaultedParamsEndNew(newArg1:newArg:)")
func defaultedParamsEnd(arg1: Int, completionHandler: @escaping (String) -> Void) { }
func defaultedParamsEndNew(newArg1: Int, newArg: Int = 0) async -> String { return "" }

@available(*, renamed: "defaultedSameLabelNew(newArg1:arg1:newArg2:)")
func defaultedSameLabel(arg1: Int, completionHandler: @escaping (String) -> Void) { }
func defaultedSameLabelNew(newArg1: Int = 0, arg1: Int = 0, newArg2: Int = 0) async -> String { return "" }

@available(*, renamed: "unlabelledArgNew(newArg1:_:newArg2:)")
func unlabelledArg(_ arg1: Int, completionHandler: @escaping (String) -> Void) { }
func unlabelledArgNew(newArg1: Int = 0, _ arg1: Int = 0, newArg2: Int = 0) async -> String { return "" }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):6 | %FileCheck -check-prefix=DEFAULTEDPARAMS %s
func defaultedParams() {
  defaultedParamsStart(arg1: 1) { str in
    print(str)
  }
  defaultedParamsMiddle(arg1: 1, arg2: 2) { str in
    print(str)
  }
  defaultedParamsEnd(arg1: 1) { str in
    print(str)
  }
  defaultedSameLabel(arg1: 1) { str in
    print(str)
  }
  unlabelledArg(1) { str in
    print(str)
  }
}
// DEFAULTEDPARAMS: func defaultedParams() async {
// DEFAULTEDPARAMS-NEXT: let str = await defaultedParamsStartNew(newArg1: 1)
// DEFAULTEDPARAMS-NEXT: print(str)
// DEFAULTEDPARAMS-NEXT: let str1 = await defaultedParamsMiddleNew(newArg1: 1, newArg2: 2)
// DEFAULTEDPARAMS-NEXT: print(str1)
// DEFAULTEDPARAMS-NEXT: let str2 = await defaultedParamsEndNew(newArg1: 1)
// DEFAULTEDPARAMS-NEXT: print(str2)
// DEFAULTEDPARAMS-NEXT: let str3 = await defaultedSameLabelNew(arg1: 1)
// DEFAULTEDPARAMS-NEXT: print(str3)
// DEFAULTEDPARAMS-NEXT: let str4 = await unlabelledArgNew(1)
// DEFAULTEDPARAMS-NEXT: print(str4)
// DEFAULTEDPARAMS-NEXT: }

struct SomeStruct {
  var instanceProp: String { get async { "" } }
  static var classProp: String { get async { "" } }

  @available(*, renamed: "simple2")
  func simple(_ completion: @escaping (String) -> Void) { }
  func simple2() async -> String { return "" }

  @available(*, renamed: "getter:instanceProp()")
  func instanceGetter(_ completion: @escaping (String) -> Void) { }
  @available(*, renamed: "getter:classProp()")
  static func classGetter(_ completion: @escaping (String) -> Void) { }
}

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):6 | %FileCheck -check-prefix=MEMBERS %s
func members(s: SomeStruct) {
  s.simple { str in
    print(str)
  }
  (((s).simple)) { str in
    print(str)
  }
  s.instanceGetter { str in
    print(str)
  }
  (((s).instanceGetter)) { str in
    print(str)
  }
  SomeStruct.classGetter { str in
    print(str)
  }
  (((SomeStruct).classGetter)) { str in
    print(str)
  }
}
// MEMBERS: func members(s: SomeStruct) async {
// MEMBERS-NEXT: let str = await s.simple2()
// MEMBERS-NEXT: print(str)
// MEMBERS-NEXT: let str1 = await (((s).simple2))()
// MEMBERS-NEXT: print(str1)
// MEMBERS-NEXT: let str2 = await s.instanceProp
// MEMBERS-NEXT: print(str2)
// MEMBERS-NEXT: let str3 = await (((s).instanceProp))
// MEMBERS-NEXT: print(str3)
// MEMBERS-NEXT: let str4 = await SomeStruct.classProp
// MEMBERS-NEXT: print(str4)
// MEMBERS-NEXT: let str5 = await (((SomeStruct).classProp))
// MEMBERS-NEXT: print(str5)
// MEMBERS-NEXT: }

@available(*, renamed: "nomatch")
func badRename(_ completion: @escaping (String) -> Void) { }
@available(*, renamed: "nomatch")
func badRenameUnlabelled(_ arg: Int, _ completion: @escaping (String) -> Void) { }
@available(*, renamed: "badRename2Async(arg:newArg:)")
func badRename2(arg: Int, completionHandler: @escaping (String) -> Void) { }
func badRename2Async(arg: Int = 0, newArg: Int) async -> String { return "" }

// Won't compile since there are no corresponding async functions
// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):6 | %FileCheck -check-prefix=FALLBACK %s
func fallback() {
  badRename { str in
    print(str)
  }
  badRenameUnlabelled(1) { str in
    print(str)
  }
  badRename2(arg: 1) { str in
    print(str)
  }
}
// FALLBACK: func fallback() async {
// FALLBACK-NEXT: let str = await badRename()
// FALLBACK-NEXT: print(str)
// FALLBACK-NEXT: let str1 = await badRenameUnlabelled(1)
// FALLBACK-NEXT: print(str1)
// FALLBACK-NEXT: let str2 = await badRename2(arg: 1)
// FALLBACK-NEXT: print(str2)
// FALLBACK-NEXT: }
