// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERRIDE_1 | %FileCheck %s -check-prefix=OVERRIDE_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERRIDE_2 | %FileCheck %s -check-prefix=OVERRIDE_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERRIDE_3 | %FileCheck %s -check-prefix=OVERRIDE_3

struct Generic<T> {}

protocol MyProtocol {
  associatedtype Result
  typealias Arg1 = Generic<Self>
  typealias Arg2<X> = Generic<X>
  typealias Func = () -> Int
  func foo<U>(arg1: Arg1, arg2: Arg2<U>, arg3: Arg2<Int>, arg4: [Arg1], arg5: Arg2<U>? arg6: Generic<Arg2<Generic<Arg2<Int>>>>) -> Result
  func bar() -> Result
  func baz(arg: @escaping Func)
}

struct MyStruct1 : MyProtocol {
  // No 'Result' witness.
  func #^OVERRIDE_1^#
}

struct MyStruct2 : MyProtocol {
  // Explicit 'Result' witness by 'typealias'.
  typealias Result = String
  func #^OVERRIDE_2^#
}

struct MyStruct3 : MyProtocol {
  // Implicit 'Result' witness by inference.
  func bar() -> String
  func #^OVERRIDE_3^#
}

// OVERRIDE_1: Begin completions, 3 items
// OVERRIDE_1-DAG: Decl[InstanceMethod]/Super: foo<U>(arg1: Arg1, arg2: Arg2<U>, arg3: Arg2<Int>, arg4: [Arg1], arg5: Arg2<U>?, arg6: Generic<Arg2<Generic<Arg2<Int>>>>) -> Result {|};
// OVERRIDE_1-DAG: Decl[InstanceMethod]/Super: bar() -> Result {|};
// OVERRIDE_1-DAG: Decl[InstanceMethod]/Super: baz(arg: @escaping Func) {|};
// OVERRIDE_1: End completions

// OVERRIDE_2: Begin completions, 3 items
// OVERRIDE_2-DAG: Decl[InstanceMethod]/Super: foo<U>(arg1: Arg1, arg2: Arg2<U>, arg3: Arg2<Int>, arg4: [Arg1], arg5: Arg2<U>?, arg6: Generic<Arg2<Generic<Arg2<Int>>>>) -> String {|};
// OVERRIDE_2-DAG: Decl[InstanceMethod]/Super: bar() -> String {|};
// OVERRIDE_2-DAG: Decl[InstanceMethod]/Super: baz(arg: @escaping Func) {|};
// OVERRIDE_2: End completions

// OVERRIDE_3-DAG: Begin completions, 2 items
// OVERRIDE_3-DAG: Decl[InstanceMethod]/Super: foo<U>(arg1: Arg1, arg2: Arg2<U>, arg3: Arg2<Int>, arg4: [Arg1], arg5: Arg2<U>?, arg6: Generic<Arg2<Generic<Arg2<Int>>>>) -> String {|};
// OVERRIDE_3-DAG: Decl[InstanceMethod]/Super: baz(arg: @escaping Func) {|};
// OVERRIDE_3-DAG: End completions

