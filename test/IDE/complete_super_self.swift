// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COVARIANT_RETURN_CONV | %FileCheck %s --check-prefix=COVARIANT_RETURN_CONV

class BaseClass {
  func returnSelf() -> Self {}
}

class DerivedClass: BaseClass {
  var value: Int
  func foo() {}
}

func test(value: DerivedClass) {
  value.returnSelf().#^COVARIANT_RETURN_CONV^#
// COVARIANT_RETURN_CONV: Begin completions, 4 items
// COVARIANT_RETURN_CONV-DAG: Keyword[self]/CurrNominal:          self[#DerivedClass#];
// COVARIANT_RETURN_CONV-DAG: Decl[InstanceVar]/CurrNominal:      value[#Int#];
// COVARIANT_RETURN_CONV-DAG: Decl[InstanceMethod]/CurrNominal:   foo()[#Void#];
// COVARIANT_RETURN_CONV-DAG: Decl[InstanceMethod]/Super:         returnSelf()[#Self#];
// COVARIANT_RETURN_CONV: End completions
}
