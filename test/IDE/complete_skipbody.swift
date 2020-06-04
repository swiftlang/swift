// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token FUNCTIONBODY -debug-forbid-typecheck-prefix FORBIDDEN | %FileCheck %s
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token TOPLEVEL -debug-forbid-typecheck-prefix FORBIDDEN | %FileCheck %s

struct FORBIDDEN_Struct {
  func FORBIDDEN_method() -> Int? { 1 }
}

struct MyStruct {
  var x: Int { 1 }
  var y: Int { 1 }
}

let globalUnrelated = FORBIDDEN_Struct();

guard let globalValueOpt = MyStruct() as MyStruct?? else {
  let localUnrelated = FORBIDDEN_Struct();
  fatalError()
}

func test(valueOptOpt: MyStruct??) {

  let FORBIDDEN_localVar = 1
  let unrelated = FORBIDDEN_Struct()

  let valueOpt = valueOptOpt!

  guard let a = unrelated.FORBIDDEN_method() else {
    return
  }

  guard let value = valueOpt else {
    let FORBIDDEN_localVar2 = 1
    return
  }

  if (value.x == 1) {
    let unrelated2 = FORBIDDEN_Struct()
    switch value.x {
    case let x where x < 2:
      let unrelated3 = FORBIDDEN_Struct()
      if x == value.#^FUNCTIONBODY^# {}
    default:
      break
    }
  }
}

let globalValue = globalValueOpt!

let FORBIDDEN_globalVar = 1

switch globalValue.x {
case let x where x < 2:
  if x == globalValue.#^TOPLEVEL^# {}
default:
  break
}

// CHECK: Begin completions, 3 items
// CHECK-DAG: Keyword[self]/CurrNominal:          self[#MyStruct#]; name=self
// CHECK-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]:      x[#Int#]; name=x
// CHECK-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]:      y[#Int#]; name=y
// CHECK: End completions
