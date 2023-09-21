// RUN: %batch-code-completion

class BaseClass {
  func returnSelf() -> Self {}
}

class DerivedClass: BaseClass {
  var value: Int
  func foo() {}

  func testWithStaticSelf() {
    self.returnSelf().#^COVARIANT_SELF_RETURN_STATIC?check=COVARIANT_SELF_RETURN^#
  }

  func testWithDynamicSelf() -> Self {
    self.returnSelf().#^COVARIANT_SELF_RETURN_DYNAMIC?check=COVARIANT_SELF_RETURN^#
    return self
  }
}

// COVARIANT_SELF_RETURN: Begin completions, 6 items
// COVARIANT_SELF_RETURN-DAG: Keyword[self]/CurrNominal:          self[#{{Self|DerivedClass}}#];
// COVARIANT_SELF_RETURN-DAG: Decl[InstanceVar]/CurrNominal:      value[#Int#];
// COVARIANT_SELF_RETURN-DAG: Decl[InstanceMethod]/CurrNominal:   foo()[#Void#];
// COVARIANT_SELF_RETURN-DAG: Decl[InstanceMethod]/Super:         returnSelf()[#Self#];
// COVARIANT_SELF_RETURN-DAG: Decl[InstanceMethod]/CurrNominal:   testWithStaticSelf()[#Void#];
// COVARIANT_SELF_RETURN-DAG: Decl[InstanceMethod]/CurrNominal:   testWithDynamicSelf()[#Self#];
