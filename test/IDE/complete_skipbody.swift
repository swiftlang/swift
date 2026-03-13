// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token FUNCTIONBODY -debug-forbid-typecheck-prefix FORBIDDEN | %FileCheck %s
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token LOCALFUNC_PARAMINIT -debug-forbid-typecheck-prefix FORBIDDEN | %FileCheck %s
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

  func localFunc(_ x: Int) -> Int {
    let FORBIDDEN_unrelatedLocal = FORBIDDEN_Struct()
    return 1
  }

  if (value.x == 1) {
    let unrelated2 = FORBIDDEN_Struct()
    switch value.x {
    case let x where x < 2:
      let unrelated3 = FORBIDDEN_Struct()
      _ = { xx in
        if xx == localFunc(value.#^FUNCTIONBODY^#) {
          let unrelated5 = FORBIDDEN_Struct()
          return 1
        }

        func innerFunc(x: Int = value.#^LOCALFUNC_PARAMINIT^#) {
          let unrelated6 = FORBIDDEN_Struct()
        }

        return 0;
      } (x)
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
// CHECK-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]:      x[#Int#]; name=x
// CHECK-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]:      y[#Int#]; name=y
