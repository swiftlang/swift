// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONTEXT | %FileCheck %s -check-prefix=CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONTEXT_VARNAME | %FileCheck %s -check-prefix=CONTEXT_VARNAME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONTEXT_STORAGE_VARNAME | %FileCheck %s -check-prefix=CONTEXT_STORAGE_VARNAME

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SELF | %FileCheck %s -check-prefix=CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SELF_VARNAME | %FileCheck %s -check-prefix=CONTEXT_VARNAME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SELF_STORAGE_VARNAME | %FileCheck %s -check-prefix=CONTEXT_STORAGE_VARNAME

@propertyWrapper
struct Lazzzy<T> {
  var wrappedValue: T
  func delegateOperation() -> Int {}
}

struct MyMember {
  var x: Int
  var y: Int
  static var zero = MyMember(x: 0, y: 0)
}

class MyClass {
  @Lazzzy<MyMember>
  var foo = .zero

  func test() {
    let _ = #^CONTEXT^#
// CONTEXT: Begin completions
// CONTEXT-DAG: Decl[InstanceVar]/CurrNominal:      foo[#MyMember#];
// CONTEXT-DAG: Decl[InstanceVar]/CurrNominal:      _foo[#Lazzzy<MyMember>#];
// CONTEXT: End completions

    let _ = foo.#^CONTEXT_VARNAME^#
// CONTEXT_VARNAME: Begin completions, 3 items
// CONTEXT_VARNAME-DAG: Keyword[self]/CurrNominal:          self[#MyMember#]; name=self
// CONTEXT_VARNAME-DAG: Decl[InstanceVar]/CurrNominal:      x[#Int#]; name=x
// CONTEXT_VARNAME-DAG: Decl[InstanceVar]/CurrNominal:      y[#Int#]; name=y
// CONTEXT_VARNAME-NOT: _
// CONTEXT_VARNAME-DAG: End completions

    let _ = _foo.#^CONTEXT_STORAGE_VARNAME^#
// CONTEXT_STORAGE_VARNAME: Begin completions, 3 items
// CONTEXT_STORAGE_VARNAME-DAG: Keyword[self]/CurrNominal:          self[#Lazzzy<MyMember>#]; name=self
// CONTEXT_STORAGE_VARNAME-DAG: Decl[InstanceVar]/CurrNominal:      wrappedValue[#MyMember#]; name=wrappedValue
// CONTEXT_STORAGE_VARNAME-DAG: Decl[InstanceMethod]/CurrNominal:   delegateOperation()[#Int#]; name=delegateOperation()
// CONTEXT_STORAGE_VARNAME-NOT: _
// CONTEXT_STORAGE_VARNAME: End completions

    let _ = self.#^SELF^#
// Same as CONTEXT.

    let _ = self.foo.#^SELF_VARNAME^#
// Same as CONTEXT_VARNAME.

    let _ = self._foo.#^SELF_STORAGE_VARNAME^#
// Same as CONTEXT_STORAGE_VARNAME.
  }
}
