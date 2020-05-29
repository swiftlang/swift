// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE -debug-forbid-typecheck-prefix FORBIDDEN | %FileCheck %s
struct FORBIDDEN_Struct {
  func FORBIDDEN_method() -> Int? { 1 }
}

struct MyStruct {
  var x: Int { 1 }
  var y: Int { 1 }
}

func test(valueOptOpt: MyStruct??) {

  let FORBIDDEN_localVar = 1
  let unrelated = FORBIDDEN_Struct()

  let valueOpt = valueOptOpt!

  guard let a = unrelated.FORBIDDEN_method() else {
    return
  }

  guard let value = valueOpt else {
    return
  }

  if (value.x == 1) {
    let unrelated2 = FORBIDDEN_Struct()
    _ = value.#^COMPLETE^#
  }
}

// CHECK: Begin completions, 3 items
// CHECK-DAG: Keyword[self]/CurrNominal:          self[#MyStruct#]; name=self
// CHECK-DAG: Decl[InstanceVar]/CurrNominal:      x[#Int#]; name=x
// CHECK-DAG: Decl[InstanceVar]/CurrNominal:      y[#Int#]; name=y
// CHECK: End completions
