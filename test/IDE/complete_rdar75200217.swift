// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=COMPLETE | %FileCheck %s

enum Encoding {
  case utf8
}

func foo(bytes: ArraySlice<UInt16>, encoding: Encoding) {
  fatalError()
}

extension Array {
  func bar<R>(r: R) -> ArraySlice<Element> where R : RangeExpression, Int == R.Bound {
    fatalError()
  }
}

/// The issue, that caused this to fail was that type checking `start` caused
/// the entire `if` condition to be type-checked, thus also type-checking `end`
/// which depends on `start`, thus creating a dependency cycle in the request
/// evaluator.
/// Thus `end` would get assigned an error type, causing `a` to be an error
/// type and thus the completion on `encoding` fails.
/// We need a particular order of type check requests to hit this behaviour,
/// that's why the test case feels over-complicated.
func deserializeName(_ data: Array<UInt16>, flag: Bool) {
  if flag, let start = Optional(Array<UInt16>.Index()), let end = Optional(start) {
    let range = start..<end
    let a = data.bar(r: range)
    foo(bytes: a, encoding: .#^COMPLETE^#)
  }
}

// CHECK-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: utf8[#Encoding#];
