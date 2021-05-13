// RUN: %empty-directory(%t)

func simple(_ completion: (String) -> Void) { }
func simple() async -> String { }

func simpleArg(arg: String, _ completion: (String) -> Void) { }
func simpleArg(arg: String) async -> String { }

func simpleErr(arg: String, _ completion: (String?, Error?) -> Void) { }
func simpleErr(arg: String) async throws -> String { }

func whatever() -> Bool { return true }

// Ideally we wouldn't rename anything here since it's correct as is, but the
// collector picks up the param `str` use in the if condition.
// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):6 | %FileCheck -check-prefix=LOCALREDECL %s
func localRedecl(str: String?) {
  if let str = str {
    print(str)
  }
  let str = "str"
  print(str)
}
// LOCALREDECL: func localRedecl(str: String?) async {
// LOCALREDECL-NEXT: if let str = str {
// LOCALREDECL-NEXT:   print(str)
// LOCALREDECL-NEXT: }
// LOCALREDECL-NEXT: let str1 = "str"
// LOCALREDECL-NEXT: print(str1)

// Again, ideally wouldn't rename as the use of `str` is above the hoisted call.
// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefixes=SHADOWUNUSEDPARAM %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+3):3 | %FileCheck -check-prefix=HOISTED-SIMPLE-CALL %s
func shadowUnusedParam(str: String) {
  print(str)
  simple { str in
    print(str)
  }
}
// SHADOWUNUSEDPARAM: func shadowUnusedParam(str: String) async {
// SHADOWUNUSEDPARAM-NEXT: print(str)
// SHADOWUNUSEDPARAM-NEXT: let str1 = await simple()
// SHADOWUNUSEDPARAM-NEXT: print(str1)

// HOISTED-SIMPLE-CALL: let str = await simple()
// HOISTED-SIMPLE-CALL-NEXT: print(str)

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=SHADOWUSEDPARAM %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+3):3 | %FileCheck -check-prefix=RENAMED-SIMPLE-CALL %s
func shadowUsedParam(str: String) {
  print(str)
  simple { str in
    print(str)
  }
  print(str)
}
// SHADOWUSEDPARAM: func shadowUsedParam(str: String) async {
// SHADOWUSEDPARAM-NEXT: print(str)
// SHADOWUSEDPARAM-NEXT: let str1 = await simple()
// SHADOWUSEDPARAM-NEXT: print(str1)
// SHADOWUSEDPARAM-NEXT: print(str)

// RENAMED-SIMPLE-CALL: let str1 = await simple()
// RENAMED-SIMPLE-CALL-NEXT: print(str1)

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=NESTEDBEFORE %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+5):5 | %FileCheck -check-prefix=HOISTED-SIMPLE-CALL %s
func nestedBefore() {
  let str = "str"
  print(str)
  if whatever() {
    simple { str in
      print(str)
    }
  }
}
// NESTEDBEFORE: func nestedBefore() async {
// NESTEDBEFORE-NEXT: let str = "str"
// NESTEDBEFORE-NEXT: print(str)
// NESTEDBEFORE-NEXT: if whatever() {
// NESTEDBEFORE-NEXT: let str = await simple()
// NESTEDBEFORE-NEXT: print(str)
// NESTEDBEFORE-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=NESTEDAFTER %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+3):5 | %FileCheck -check-prefix=HOISTED-SIMPLE-CALL %s
func nestedAfter() {
  if whatever() {
    simple { str in
      print(str)
    }
  }
  let str = "str"
  print(str)
}
// NESTEDAFTER: func nestedAfter() async {
// NESTEDAFTER-NEXT: if whatever() {
// NESTEDAFTER-NEXT: let str = await simple()
// NESTEDAFTER-NEXT: print(str)
// NESTEDAFTER-NEXT: }
// NESTEDAFTER-NEXT: let str = "str"
// NESTEDAFTER-NEXT: print(str)

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=NESTED-DECL-BEFORE %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+4):5 | %FileCheck -check-prefix=RENAMED-SIMPLE-CALL %s
func nestedDeclBefore() {
  if whatever() {
    let str = "str"
    simple { str in
      print(str)
    }
  }
}
// NESTED-DECL-BEFORE: func nestedDeclBefore() async {
// NESTED-DECL-BEFORE-NEXT: if whatever() {
// NESTED-DECL-BEFORE-NEXT: let str = "str"
// NESTED-DECL-BEFORE-NEXT: let str1 = await simple()
// NESTED-DECL-BEFORE-NEXT: print(str1)
// NESTED-DECL-BEFORE-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=NESTED-DECL-AFTER %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+3):5 | %FileCheck -check-prefix=RENAMED-SIMPLE-CALL %s
func nestedDeclAfter() {
  if whatever() {
    simple { str in
      print(str)
    }
    let str = "str"
  }
}
// NESTED-DECL-AFTER: func nestedDeclAfter() async {
// NESTED-DECL-AFTER-NEXT: if whatever() {
// NESTED-DECL-AFTER-NEXT: let str = await simple()
// NESTED-DECL-AFTER-NEXT: print(str)
// NESTED-DECL-AFTER-NEXT: let str1 = "str"
// NESTED-DECL-AFTER-NEXT: }

// Ideally wouldn't rename, but is for the same reason as before
// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=NESTED-USE-BEFORE %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+5):5 | %FileCheck -check-prefix=HOISTED-SIMPLE-CALL %s
func nestedUseBefore() {
  let str = "str"
  if whatever() {
    print(str)
    simple { str in
      print(str)
    }
  }
}
// NESTED-USE-BEFORE: func nestedUseBefore() async {
// NESTED-USE-BEFORE-NEXT: let str = "str"
// NESTED-USE-BEFORE-NEXT: if whatever() {
// NESTED-USE-BEFORE-NEXT: print(str)
// NESTED-USE-BEFORE-NEXT: let str1 = await simple()
// NESTED-USE-BEFORE-NEXT: print(str1)
// NESTED-USE-BEFORE-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=NESTED-USE-AFTER %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+4):5 | %FileCheck -check-prefix=RENAMED-SIMPLE-CALL %s
func nestedUseAfter() {
  let str = "str"
  if whatever() {
    simple { str in
      print(str)
    }
    print(str)
  }
}
// NESTED-USE-AFTER: func nestedUseAfter() async {
// NESTED-USE-AFTER-NEXT: let str = "str"
// NESTED-USE-AFTER-NEXT: if whatever() {
// NESTED-USE-AFTER-NEXT: let str1 = await simple()
// NESTED-USE-AFTER-NEXT: print(str1)
// NESTED-USE-AFTER-NEXT: print(str)
// NESTED-USE-AFTER-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=REDECLBEFORE %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+4):3 | %FileCheck -check-prefix=RENAMED-SIMPLE-CALL %s
func redeclBefore() {
  let str = "do not redecl"
  print(str)
  simple { str in
    print(str)
  }
}
// REDECLBEFORE: func redeclBefore() async {
// REDECLBEFORE-NEXT: let str = "do not redecl"
// REDECLBEFORE-NEXT: print(str)
// REDECLBEFORE-NEXT: let str1 = await simple()
// REDECLBEFORE-NEXT: print(str1)

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=GUARDREDECLBEFORE %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+6):3 | %FileCheck -check-prefix=RENAMED-SIMPLE-CALL %s
func guardRedeclBefore(arg: String?) {
  guard let str = arg else {
    return
  }
  print(str)
  simple { str in
    print(str)
  }
}
// GUARDREDECLBEFORE: func guardRedeclBefore(arg: String?) async {
// GUARDREDECLBEFORE-NEXT: guard let str = arg else {
// GUARDREDECLBEFORE-NEXT: return
// GUARDREDECLBEFORE-NEXT: }
// GUARDREDECLBEFORE-NEXT: print(str)
// GUARDREDECLBEFORE-NEXT: let str1 = await simple()
// GUARDREDECLBEFORE-NEXT: print(str1)

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=IFDECLBEFORE %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+5):3 | %FileCheck -check-prefix=HOISTED-SIMPLE-CALL %s
func ifDeclBefore(arg: String?) {
  if let str = arg {
    print(str)
  }
  simple { str in
    print(str)
  }
}
// IFDECLBEFORE: func ifDeclBefore(arg: String?) async {
// IFDECLBEFORE-NEXT: if let str = arg {
// IFDECLBEFORE-NEXT: print(str)
// IFDECLBEFORE-NEXT: }
// IFDECLBEFORE-NEXT: let str = await simple()
// IFDECLBEFORE-NEXT: print(str)

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=REDECLAFTER %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=RENAMED-SIMPLE-CALL %s
func redeclAfter() {
  simple { str in
    print(str)
  }
  let str = "do not redecl"
  print(str)
}
// REDECLAFTER: func redeclAfter() async {
// REDECLAFTER-NEXT: let str = await simple()
// REDECLAFTER-NEXT: print(str)
// REDECLAFTER-NEXT: let str1 = "do not redecl"
// REDECLAFTER-NEXT: print(str1)

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=GUARDREDECLAFTER %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=RENAMED-SIMPLE-CALL %s
func guardRedeclAfter(arg: String?) {
  simple { str in
    print(str)
  }
  guard let str = arg else {
    return
  }
  print(str)
}
// GUARDREDECLAFTER: func guardRedeclAfter(arg: String?) async {
// GUARDREDECLAFTER-NEXT: let str = await simple()
// GUARDREDECLAFTER-NEXT: print(str)
// GUARDREDECLAFTER-NEXT: guard let str1 = arg else {
// GUARDREDECLAFTER-NEXT: return
// GUARDREDECLAFTER-NEXT: }
// GUARDREDECLAFTER-NEXT: print(str1)

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=IFDECLAFTER %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=HOISTED-SIMPLE-CALL %s
func ifDeclAfter(arg: String?) {
  simple { str in
    print(str)
  }
  if let str = arg {
    print(str)
  }
}
// IFDECLAFTER: func ifDeclAfter(arg: String?) async {
// IFDECLAFTER-NEXT: let str = await simple()
// IFDECLAFTER-NEXT: print(str)
// IFDECLAFTER-NEXT: if let str = arg {
// IFDECLAFTER-NEXT: print(str)
// IFDECLAFTER-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=REDECLINNER %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=REDECLINNER %s
func redeclInner() {
  simple { str in
    simpleArg(arg: str) { other in
      let str = other
      print(str)
    }
  }
}
// REDECLINNER: let str = await simple()
// REDECLINNER-NEXT: let other = await simpleArg(arg: str)
// REDECLINNER-NEXT: let str1 = other
// REDECLINNER-NEXT: print(str1)

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=DECLINNER %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=DECLINNER %s
func declInner() {
  simple { str in
    simpleArg(arg: str) { other in
      if other == "anything" {
        let str = other
        print(str)
      }
    }
  }
}
// DECLINNER: let str = await simple()
// DECLINNER-NEXT: let other = await simpleArg(arg: str)
// DECLINNER-NEXT: if other == "anything" {
// DECLINNER-NEXT: let str = other
// DECLINNER-NEXT: print(str)
// DECLINNER-NEXT: }

// TODO: `throws` isn't added to the function declaration
// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=REDECLHOISTED %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=REDECLHOISTED %s
func redeclInnerHoisted() {
  simple { str in
    simpleErr(arg: str) { other, err in
      if let other = other {
        let str = other
        print(str)
      }
    }
  }
}
// REDECLHOISTED: let str = await simple()
// REDECLHOISTED-NEXT: let other = try await simpleErr(arg: str)
// REDECLHOISTED-NEXT: let str1 = other
// REDECLHOISTED-NEXT: print(str1)

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=SHADOWINNER %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=SHADOWINNER %s
func shadowInner() {
  simple { str in
    simpleArg(arg: str) { str in
      print(str)
    }
  }
}
// SHADOWINNER: let str = await simple()
// SHADOWINNER-NEXT: let str1 = await simpleArg(arg: str)
// SHADOWINNER-NEXT: print(str1)

// TODO: `throws` isn't added to the function declaration
// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=SHADOWINNERBIND %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=SHADOWINNERBIND %s
func shadowInnerBind() {
  simple { str in
    simpleErr(arg: str) { other, err in
      if let str = other {
        print(str)
      }
    }
  }
}
// SHADOWINNERBIND: let str = await simple()
// SHADOWINNERBIND-NEXT: let str1 = try await simpleErr(arg: str)
// SHADOWINNERBIND-NEXT: print(str1)

// TODO: `throws` isn't added to the function declaration
// RUN: %refactor  -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=SHADOWNAMEEXISTS %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=SHADOWNAMEEXISTS %s
func shadowNameAlreadyExists() {
  simple { str in
    simpleErr(arg: str) { str, err in
      let str1 = "str1"
      print(str1)
      if let str1 = str {
        print(str1)
      }
      if let str2 = str {
        print(str2)
      }
    }
  }
}
// SHADOWNAMEEXISTS: let str = await simple()
// SHADOWNAMEEXISTS-NEXT: let str1 = try await simpleErr(arg: str)
// SHADOWNAMEEXISTS-NEXT: let str11 = "str1"
// SHADOWNAMEEXISTS-NEXT: print(str11)
// SHADOWNAMEEXISTS-NEXT: print(str1)
// SHADOWNAMEEXISTS-NEXT: print(str1)

func shadowsUsedDecl() {
  let inOuter: String = "str"
  print(inOuter)
  // TODO: `throws` isn't added to the function declaration
  // RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+2):8 | %FileCheck -check-prefix=SHADOWOUTERUSED %s
  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):10 | %FileCheck -check-prefix=SHADOWOUTERUSED %s
  func inner() {
    simpleErr(arg: "str") { inCall, err in
      if inCall != nil {
        let inOuter = inCall!
        print(inOuter)
      }
      print(inOuter)
    }
  }
}
// SHADOWOUTERUSED: let inCall = try await simpleErr(arg: "str")
// SHADOWOUTERUSED-NEXT: let inOuter1 = inCall
// SHADOWOUTERUSED-NEXT: print(inOuter1)
// SHADOWOUTERUSED-NEXT: print(inOuter)

func shadowsUnusedDecl() {
  let inOuter: String = "str"
  print(inOuter)
  // TODO: `throws` isn't added to the function declaration
  // RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+2):8 | %FileCheck -check-prefix=SHADOWOUTERUNUSED %s
  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):10 | %FileCheck -check-prefix=SHADOWOUTERUNUSED %s
  func inner() {
    simpleErr(arg: "str") { inCall, err in
      if inCall != nil {
        let inOuter = inCall!
        print(inOuter)
      }
    }
  }
}
// SHADOWOUTERUNUSED: let inCall = try await simpleErr(arg: "str")
// SHADOWOUTERUNUSED-NEXT: let inOuter = inCall
// SHADOWOUTERUNUSED-NEXT: print(inOuter)

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=IGNORE-SCOPED-BEFORE %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+16):3 | %FileCheck -check-prefix=HOISTED-SIMPLE-CALL %s
func ignoreScopedBefore(arg: String?, args: [String]) {
  if let str = arg {
    print(str)
  }
  for str in args {
    print(str)
  }
  var check = arg
  while let str = check {
    check = str
  }
  do {
    let str = arg!
    print(str)
  }
  simple { str in
    print(str)
  }
}
// IGNORE-SCOPED-BEFORE: if let str = arg {
// IGNORE-SCOPED-BEFORE-NEXT: print(str)
// IGNORE-SCOPED-BEFORE-NEXT: }
// IGNORE-SCOPED-BEFORE-NEXT: for str in args {
// IGNORE-SCOPED-BEFORE-NEXT: print(str)
// IGNORE-SCOPED-BEFORE-NEXT: }
// IGNORE-SCOPED-BEFORE-NEXT: var check = arg
// IGNORE-SCOPED-BEFORE-NEXT: while let str = check {
// IGNORE-SCOPED-BEFORE-NEXT:   check = str
// IGNORE-SCOPED-BEFORE-NEXT: }
// IGNORE-SCOPED-BEFORE-NEXT: do {
// IGNORE-SCOPED-BEFORE-NEXT: let str = arg!
// IGNORE-SCOPED-BEFORE-NEXT: print(str)
// IGNORE-SCOPED-BEFORE-NEXT: }
// IGNORE-SCOPED-BEFORE-NEXT: let str = await simple()
// IGNORE-SCOPED-BEFORE-NEXT: print(str)

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=IGNORE-SCOPED-AFTER %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=HOISTED-SIMPLE-CALL %s
func ignoreScopedAfter(arg: String?, args: [String]) {
  simple { str in
    print(str)
  }
  if let str = arg {
    print(str)
  }
  for str in args {
    print(str)
  }
  var check = arg
  while let str = check {
    check = str
  }
  do {
    let str = arg!
    print(str)
  }
}
// IGNORE-SCOPED-AFTER: let str = await simple()
// IGNORE-SCOPED-AFTER-NEXT: print(str)
// IGNORE-SCOPED-AFTER-NEXT: if let str = arg {
// IGNORE-SCOPED-AFTER-NEXT: print(str)
// IGNORE-SCOPED-AFTER-NEXT: }
// IGNORE-SCOPED-AFTER-NEXT: for str in args {
// IGNORE-SCOPED-AFTER-NEXT: print(str)
// IGNORE-SCOPED-AFTER-NEXT: }
// IGNORE-SCOPED-AFTER-NEXT: var check = arg
// IGNORE-SCOPED-AFTER-NEXT: while let str = check {
// IGNORE-SCOPED-AFTER-NEXT:   check = str
// IGNORE-SCOPED-AFTER-NEXT: }
// IGNORE-SCOPED-AFTER-NEXT: do {
// IGNORE-SCOPED-AFTER-NEXT: let str = arg!
// IGNORE-SCOPED-AFTER-NEXT: print(str)
// IGNORE-SCOPED-AFTER-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefixes=TYPE-BEFORE,TYPE-BEFORE-CALL %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+3):3 | %FileCheck -check-prefix=TYPE-BEFORE-CALL %s
func typeBefore() {
  struct Foo {}
  simple { Foo in
    print(Foo)
  }
}
// TYPE-BEFORE: struct Foo {}
// TYPE-BEFORE-CALL: let Foo1 = await simple()
// TYPE-BEFORE-CALL-NEXT: print(Foo1)

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefixes=FUNC-BEFORE,FUNC-BEFORE-CALL %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+3):3 | %FileCheck -check-prefix=FUNC-BEFORE-CALL %s
func funcBefore() {
  func foo() {}
  simple { foo in
    print(foo)
  }
}
// FUNC-BEFORE: func foo() {}
// FUNC-BEFORE-CALL: let foo1 = await simple()
// FUNC-BEFORE-CALL-NEXT: print(foo1)

enum SomeEnum {
  case foo(String)
  case bar(String)
  case baz(String)
}

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+2):6 | %FileCheck -check-prefix=CASE-SCOPES %s
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+15):5 | %FileCheck -check-prefix=HOISTED-SIMPLE-CALL %s
func caseScopes() {
  switch SomeEnum.foo("a") {
  case .foo(let arg):
    simple { str in
      print(str)
    }
  case .bar(let str):
    simple { str in
      print(str)
    }
  case .baz(let arg):
    simple { str in
      print(str)
    }
    simple { str in
      print(str)
    }
  }
}
// CASE-SCOPES: func caseScopes() async {
// CASE-SCOPES-NEXT: switch SomeEnum.foo("a") {
// CASE-SCOPES-NEXT: case .foo(let arg):
// CASE-SCOPES-NEXT: let str = await simple()
// CASE-SCOPES-NEXT: print(str)
// CASE-SCOPES-NEXT: case .bar(let str):
// CASE-SCOPES-NEXT: let str = await simple()
// CASE-SCOPES-NEXT: print(str)
// CASE-SCOPES-NEXT: case .baz(let arg):
// CASE-SCOPES-NEXT: let str = await simple()
// CASE-SCOPES-NEXT: print(str)
// CASE-SCOPES-NEXT: let str1 = await simple()
// CASE-SCOPES-NEXT: print(str1)
// CASE-SCOPES-NEXT: }
// CASE-SCOPES-NEXT: }

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):1 | %FileCheck -check-prefix=TOP-LEVEL-VAR %s
let inFile = "inFile"
simple { inFile in
  print(inFile)
}
// TOP-LEVEL-VAR: let inFile1 = await simple()
// TOP-LEVEL-VAR-NEXT: print(inFile1)

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):1 | %FileCheck -check-prefix=TOP-LEVEL-FUNC %s
func fileFunc() {}
simple { fileFunc in
  print(fileFunc)
}
// TOP-LEVEL-FUNC: let fileFunc1 = await simple()
// TOP-LEVEL-FUNC-NEXT: print(fileFunc1)

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):1 | %FileCheck -check-prefix=TOP-LEVEL-PROTO %s
protocol FileProto {}
simple { FileProto in
  print(FileProto)
}
// TOP-LEVEL-PROTO: let FileProto1 = await simple()
// TOP-LEVEL-PROTO-NEXT: print(FileProto1)

// The following results in two TopLevelCodeDecls each with their own BraceStmt,
// we want to make sure that we still find the `someGlobal` reference and thus
// rename the `someGlobal` closure arg.
let someGlobal = "someGlobal"
func between1() {}
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=TOP-LEVEL-REFERENCE %s
simple { someGlobal in
  print(someGlobal)
}
func between2() {}
print(someGlobal)
// TOP-LEVEL-REFERENCE: let someGlobal1 = await simple()
// TOP-LEVEL-REFERENCE-NEXT: print(someGlobal1)
