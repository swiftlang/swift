// REQUIRES: concurrency
// REQUIRES: objc_interop

// RUN: %target-typecheck-verify-swift -disable-availability-checking -verify-ignore-unknown -I %S/Inputs/custom-modules
// RUN: %target-typecheck-verify-swift -disable-availability-checking -verify-ignore-unknown -parse-as-library -I %S/Inputs/custom-modules

import ObjcAsync

// The following should match to a corresponding decl

@available(*, renamed: "asyncFunc(_:)")
func goodFunc1(value: String, completionHandler: @escaping (Int) -> Void) {}
@available(*, renamed: "asyncFunc(_:)")
func goodFunc2(value: String, completionHandler: @escaping (Int) -> Void) {}
// expected-note@+1 4 {{'asyncFunc' declared here}}
func asyncFunc(_ text: String) async -> Int { }

// Ambiguous but only one is async
@available(*, renamed: "overloaded()")
func asyncOnlyOverload(completionHandler: @escaping () -> Void) { }
func overloaded() { }
// expected-note@+1 {{'overloaded()' declared here}}
func overloaded() async { }

// Renamed decl is ambiguous but the params only match a single case
@available(*, renamed: "overloadedAsyncFunc(value:)")
func nonAmbiguousFunc(value: Int, handler: @escaping () -> Void) {}
// expected-note@+1 {{'overloadedAsyncFunc(value:)' declared here}}
func overloadedAsyncFunc(value: Int) async {}
func overloadedAsyncFunc(value: String) async {}

// More parameters in async but they have defaults and different labels
@available(*, renamed: "defaultedParamsStart(newArg:arg:)")
func defaultedParamsStart(arg: Int, completionHandler: @escaping () -> Void) { }
// expected-note@+1 {{'defaultedParamsStart(newArg:arg:)' declared here}}
func defaultedParamsStart(newArg: String = "", arg: Int) async { }

@available(*, renamed: "defaultedParamsStart2(newArg:arg:)")
func defaultedParamsStart2(arg: Int, completionHandler: @escaping () -> Void) { }
// expected-note@+1 {{'defaultedParamsStart2(newArg:arg:)' declared here}}
func defaultedParamsStart2(newArg: Int = 0, arg: Int) async { }

@available(*, renamed: "defaultedParamsMiddle(arg1:newArg:arg2:)")
func defaultedParamsMiddle(arg1: Int, arg2: Int, completionHandler: @escaping () -> Void) { }
// expected-note@+1 {{'defaultedParamsMiddle(arg1:newArg:arg2:)' declared here}}
func defaultedParamsMiddle(arg1: Int, newArg: String = "", arg2: Int) async { }

@available(*, renamed: "defaultedParamsMiddle2(arg1:newArg:arg2:)")
func defaultedParamsMiddle2(arg1: Int, arg2: Int, completionHandler: @escaping () -> Void) { }
// expected-note@+1 {{'defaultedParamsMiddle2(arg1:newArg:arg2:)' declared here}}
func defaultedParamsMiddle2(arg1: Int, newArg: Int = 0, arg2: Int) async { }

@available(*, renamed: "defaultedParamsEnd(arg:newArg:)")
func defaultedParamsEnd(arg: Int, completionHandler: @escaping () -> Void) { }
// expected-note@+1 {{'defaultedParamsEnd(arg:newArg:)' declared here}}
func defaultedParamsEnd(arg: Int, newArg: String = "") async { }

@available(*, renamed: "defaultedParamsEnd2(arg:newArg:)")
func defaultedParamsEnd2(arg: Int, completionHandler: @escaping () -> Void) { }
// expected-note@+1 {{'defaultedParamsEnd2(arg:newArg:)' declared here}}
func defaultedParamsEnd2(arg: Int, newArg: Int = 0) async { }

@available(*, renamed: "defaultedParamsEnd3(newArg:arg:)")
func defaultedParamsEnd3(arg: Int, completionHandler: @escaping () -> Void) { }
// expected-note@+1 {{'defaultedParamsEnd3(newArg:arg:)' declared here}}
func defaultedParamsEnd3(newArg: Int, arg: String = "") async { }

@available(*, renamed: "defaultedParamsEnd4(newArg:arg:)")
func defaultedParamsEnd4(arg: Int, completionHandler: @escaping () -> Void) { }
// expected-note@+1 {{'defaultedParamsEnd4(newArg:arg:)' declared here}}
func defaultedParamsEnd4(newArg: Int, arg: Int = 0) async { }

@available(*, deprecated)
@available(macOS, introduced: 12, renamed: "manyAttrsOld()")
@available(*, renamed: "manyAttrsNew()")
@available(*, renamed: "manyAttrsNewOther()")
@available(macOS, deprecated: 12, renamed: "manyAttrsOld()")
@available(*, deprecated)
func manyAttrs(completionHandler: @escaping () -> Void) { }
// expected-note@+1 {{'manyAttrsNew()' declared here}}
func manyAttrsNew() async { }

@available(macOS, introduced: 12, renamed: "platformOnlyNew()")
func platformOnly(completionHandler: @escaping () -> Void) { }
// expected-note@+1 {{'platformOnlyNew()' declared here}}
func platformOnlyNew() async { }

struct AnotherStruct {
  var otherInstanceProp: Int { get async { 1 } }
}

struct SomeStruct {
  @available(*, renamed: "structFunc")
  func structFunc(continuation: @escaping () -> Void) { }

  // expected-note@+1{{'structFunc()' declared here}}
  func structFunc() async { }

  @available(*, renamed: "staticStructFunc")
  static func staticStructFunc(completionHandler: @escaping () -> Void) { }

  // expected-note@+1 2 {{'staticStructFunc()' declared here}}
  static func staticStructFunc() async { }

  // expected-note@+1 3 {{'getter:instanceProp()' declared here}}
  var instanceProp: Int { get async { 1 } }
  var regInstanceProp: Int { get { 1 } set { } }
  // expected-note@+1 {{'getter:classProp()' declared here}}
  static var classProp: Int { get async { 1 } }

  @available(*, renamed: "getter:instanceProp()")
  func instanceGetter(completion: @escaping (Int) -> Void) { }
  @available(*, renamed: "getter:classProp()")
  static func classGetter(completion: @escaping (Int) -> Void) { }
  @available(*, renamed: "getter:instanceProp(a:b:)")
  func argsIgnored(completion: @escaping (Int) -> Void) { }
  @available(*, renamed: "getter:DoesNotExist.instanceProp()")
  func baseIgnored(completion: @escaping (Int) -> Void) { }

  @available(*, renamed: "instanceProp()")
  func noPrefix(completion: @escaping (Int) -> Void) { }
  @available(*, renamed: "getter:instanceProp()")
  func argMismatch(arg: Int, completion: @escaping (Int) -> Void) { }
  @available(*, renamed: "setter:regInstanceProp(newValue:)")
  func instanceSetter(arg: Int, completion: @escaping (Int) -> Void) { }

  @available(*, renamed: "getter:AnotherStruct.otherInstanceProp()")
  func otherInstance(completion: @escaping (Int) -> Void) { }
}


// The following should *not* match to a corresponding decl

// Renamed decl doesn't match any function
@available(*, renamed: "asyncFunc()")
func badFunc(value: String, completionHandler: @escaping (Int) -> Void) {}

// Not a completion handler
@available(*, renamed: "notCompletionRenamed()")
func notCompletion() {}
func notCompletionRenamed() async {}

// Corresponding function isn't async
@available(*, renamed: "completionNotAsyncRenamed()")
func completionNotAsync(completionHandler: @escaping () -> Void) {}
func completionNotAsyncRenamed() {}

// Renamed decl is ambiguous and there's multiple matches
@available(*, renamed: "asyncFuncDifferentParamNames")
func ambiguousFunc(value: Int, handler: @escaping () -> Void) {}
func asyncFuncDifferentParamNames(value: Int) async {}
func asyncFuncDifferentParamNames(value2: Int) async {}

// Renamed decl doesn't have enough params
@available(*, renamed: "fewerParamsFunc()")
func fewerParamsFunc(value: Int, handler: @escaping () -> Void) {}
func fewerParamsFunc() async {}

// Renamed decl has more params
@available(*, renamed: "moreParamsFunc()")
func moreParamsFunc(handler: @escaping () -> Void) {}
func moreParamsFunc(value: Int) async {}

// Renamed decl params types don't match
@available(*, renamed: "noMatchingParamsIntFunc(value:)")
func noMatchingParamsFunc(value: Character, handler: @escaping () -> Void) {}
func noMatchingParamsIntFunc(value: Int) async {}

// Matching function isn't async
@available(*, renamed: "noMatchingSyncFunc(value:)")
func noMatchingAsyncFunc(value: Int, handler: @escaping () -> Void) {}
func noMatchingSyncFunc(value: Int) {}

@available(*, renamed: "sameLabelsDifferentOrder(arg2:arg:)")
func sameLabelsDifferentOrder(arg: Int, arg2: String, completionHandler: @escaping () -> Void) { }
func sameLabelsDifferentOrder(arg2: String, arg: Int) async { }

@available(*, renamed: "handlerNotRemoved(newArg:completionHandler:)")
func handlerNotRemoved(arg: Int, completionHandler: @escaping () -> Void) {}
func handlerNotRemoved(newArg: Int, completionHandler: @escaping () -> Void) async {}

// Extra arguments. Even though there's defaults, they match the previous
// labels so they shouldn't be skipped. Thus the functions do not match.
@available(*, renamed: "defaultedParamsStartBad(arg:newArg:)")
func defaultedParamsStartBad(arg: Int, completionHandler: @escaping () -> Void) { }
func defaultedParamsStartBad(arg: String = "", newArg: Int) async { }

@available(*, renamed: "defaultedParamsStartBad2(arg:newArg:)")
func defaultedParamsStartBad2(arg: Int, completionHandler: @escaping () -> Void) { }
func defaultedParamsStartBad2(arg: Int = 0, newArg: Int) async { }

@available(*, renamed: "defaultedParamsMiddleBad(arg1:arg2:newArg:)")
func defaultedParamsMiddleBad(arg1: Int, arg2: Int, completionHandler: @escaping () -> Void) { }
func defaultedParamsMiddleBad(arg1: Int, arg2: String = "", newArg: Int) async { }

@available(*, renamed: "defaultedParamsMiddleBad2(arg1:arg2:newArg:)")
func defaultedParamsMiddleBad2(arg1: Int, arg2: Int, completionHandler: @escaping () -> Void) { }
func defaultedParamsMiddleBad2(arg1: Int, arg2: Int = 0, newArg: Int) async { }


// Suggest using async alternative function in async context

func asyncContext(t: HandlerTest) async {
  // expected-warning@+1{{consider using asynchronous alternative function}}
  goodFunc1(value: "Hello") { _ in }

  let _ = {
    // No warning or error since we're in a sync context here
    goodFunc1(value: "Hello") { _ in }
  }

  let _ = { () async -> () in
    let _ = await asyncFunc("Hello World")
    // expected-warning@+1{{consider using asynchronous alternative function}}
    goodFunc1(value: "Hello") { _ in }
  }

  let _ = await asyncFunc("World")

  // expected-warning@+1{{consider using asynchronous alternative function}}
  asyncOnlyOverload() { }
  // expected-warning@+1{{consider using asynchronous alternative function}}
  nonAmbiguousFunc(value: 1) { }
  // expected-warning@+1{{consider using asynchronous alternative function}}
  defaultedParamsStart(arg: 1) { }
  // expected-warning@+1{{consider using asynchronous alternative function}}
  defaultedParamsStart2(arg: 1) { }
  // expected-warning@+1{{consider using asynchronous alternative function}}
  defaultedParamsMiddle(arg1: 1, arg2: 2) { }
  // expected-warning@+1{{consider using asynchronous alternative function}}
  defaultedParamsMiddle2(arg1: 1, arg2: 2) { }
  // expected-warning@+1{{consider using asynchronous alternative function}}
  defaultedParamsEnd(arg: 1) { }
  // expected-warning@+1{{consider using asynchronous alternative function}}
  defaultedParamsEnd2(arg: 1) { }
  // expected-warning@+1{{consider using asynchronous alternative function}}
  defaultedParamsEnd3(arg: 1) { }
  // expected-warning@+1{{consider using asynchronous alternative function}}
  defaultedParamsEnd4(arg: 1) { }
  // expected-warning@+1{{consider using asynchronous alternative function}}
  manyAttrs() { }
  // expected-warning@+1{{consider using asynchronous alternative function}}
  platformOnly() { }

  // These don't get the warning because we failed to resolve the name to a
  // single async decl
  badFunc(value: "Hello") { _ in }
  notCompletion()
  completionNotAsync() { }
  ambiguousFunc(value: 1) { }
  fewerParamsFunc(value: 1) { }
  moreParamsFunc() { }
  noMatchingParamsFunc(value: "c") { }
  noMatchingAsyncFunc(value: 1) { }
  sameLabelsDifferentOrder(arg: 1, arg2: "") { }
  handlerNotRemoved(arg: 1) { }
  defaultedParamsStartBad(arg: 1) { }
  defaultedParamsStartBad2(arg: 1) { }
  defaultedParamsMiddleBad(arg1: 1, arg2: 2) { }
  defaultedParamsMiddleBad2(arg1: 1, arg2: 2) { }

  // expected-warning@+1{{consider using asynchronous alternative function}}
  t.simple { _ in }
  _ = await t.simple()

  // expected-warning@+1{{consider using asynchronous alternative function}}
  t.simpleArg(1) { _ in }
  _ = await t.simpleArg(1)

  // expected-warning@+1{{consider using asynchronous alternative function}}
  t.alias { _ in }
  _ = await t.alias()

  // expected-warning@+1{{consider using asynchronous alternative function}}
  t.error { _, _ in }
  _ = try! await t.error()

  // expected-warning@+1{{consider using asynchronous alternative function}}
  try! t.removedError { _, _ in }
  _ = try! await t.removedError()

  // expected-warning@+1{{consider using asynchronous alternative function}}
  t.asyncImportSame(1, completionHandler: { _ in })
  _ = await t.asyncImportSame(1)

  // Marked with swift_async(none), so shouldn't have a warning about using it
  t.asyncImportSame(1, replyTo: { _ in })
}

func syncContext(t: HandlerTest) {
  goodFunc1(value: "Hello") { _ in }
  t.simple { _ in }
  t.simpleArg(1) { _ in }
  t.alias { _ in }
  t.error { _, _ in }
  try! t.removedError { _, _ in }
  t.asyncImportSame(1, completionHandler: { _ in })
  t.asyncImportSame(1, replyTo: { _ in })
}

let asyncGlobalClosure = { () async -> () in
  // expected-warning@+1{{consider using asynchronous alternative function}}
  goodFunc1(value: "neat") { _ in }
}

class ClassCallingAsyncStuff {
  struct NestedStruct {
    @available(*, renamed: "structFunc()")
    func structCompFunc(handler: @escaping () -> ()) { }

    // expected-note@+1{{'structFunc()' declared here}}
    func structFunc() async {}
  }

  // expected-note@+1 4 {{'asyncFunc()' declared here}}
  func asyncFunc() async {}

  @available(*, renamed: "asyncFunc()")
  func compHandlerFunc(handler: @escaping () -> ()) {}

  @available(*, renamed: "asyncFunc()")
  func compAsyncHandlerFunc(handler: @escaping () async -> ()) {}

  func async1() async {
    // expected-warning@+1{{consider using asynchronous alternative function}}
    goodFunc1(value: "hi") { _ in }

    // expected-warning@+1{{consider using asynchronous alternative function}}
    compAsyncHandlerFunc() { [self] () async -> () in
      // expected-warning@+1{{consider using asynchronous alternative function}}
      compAsyncHandlerFunc() { [self] () async -> () in
        // expected-warning@+1{{consider using asynchronous alternative function}}
        compHandlerFunc() { print("foo") }
      }
    }
  }

  func instanceFunc(other: ClassCallingAsyncStuff) async {
    // expected-error@+1{{cannot find 'c' in scope}}
    c.compHandlerFunc() { }

    // expected-warning@+1{{consider using asynchronous alternative function}}
    other.compHandlerFunc() { }
  }

  func structFunc(other: NestedStruct) async {
    // expected-warning@+1{{consider using asynchronous alternative function}}
    other.structCompFunc() { }
  }

  func structFunc(other: SomeStruct) async {
    // expected-warning@+1{{consider using asynchronous alternative function}}
    other.structFunc() { }

    // expected-warning@+1{{consider using asynchronous alternative function}}
    SomeStruct.staticStructFunc { }

    // expected-warning@+1{{consider using asynchronous alternative function}}
    type(of: other).staticStructFunc { }

    // expected-warning@+1{{consider using asynchronous alternative function}}
    other.instanceGetter { _ in }
    // expected-warning@+1{{consider using asynchronous alternative function}}
    other.argsIgnored { _ in }
    // expected-warning@+1{{consider using asynchronous alternative function}}
    other.baseIgnored { _ in }
    // expected-warning@+1{{consider using asynchronous alternative function}}
    SomeStruct.classGetter { _ in }

    other.noPrefix { _ in }
    other.argMismatch(arg: 1) { _ in }
    other.instanceSetter(arg: 1) { _ in }
    other.otherInstance { _ in }
  }

  // no warning
  let funFunc = goodFunc1
}
