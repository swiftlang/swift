// RUN: %batch-code-completion

protocol P {}

class CompleteWeak {
  func instanceFunc() {}

  func weakVars() {
    weak var weakSelf = self
    unowned var unownedSelf = self
    #^WEAK_VARS_1^#
  }
// WEAK_VARS_1-DAG: Decl[LocalVar]/Local: weakSelf[#CompleteWeak?#]{{; name=.+$}}
// WEAK_VARS_1-DAG: Decl[LocalVar]/Local: unownedSelf[#CompleteWeak#]{{; name=.+$}}

  func weakNoDot() {
    weak var weakSelf = self
    weakSelf#^WEAK_NO_DOT_1^#
  }
// WEAK_NO_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: ?.instanceFunc()[#Void#]{{; name=.+$}}

  func weakDot() {
    weak var weakSelf = self
    weakSelf.#^WEAK_DOT_1^#
  }
// WEAK_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal/Erase[1]: ?.instanceFunc()[#Void#]{{; name=.+$}}

  func unownedNoDot() {
    unowned var unownedSelf = self
    unownedSelf#^UNOWNED_NO_DOT_1^#
  }
// UNOWNED_NO_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: .instanceFunc()[#Void#]{{; name=.+$}}

  func unownedDot() {
    unowned var unownedSelf = self
    unownedSelf.#^UNOWNED_DOT_1^#
  }
// UNOWNED_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc()[#Void#]{{; name=.+$}}

  func takesSomeP(_ x: some P) {}

  func weakInGenericArg() {
    // Make sure we correctly strip the weak reference type here when computing type relation.
    weak var weakSelf = self
    takesSomeP(#^WEAK_IN_GENERIC_ARG^#)
    // WEAK_IN_GENERIC_ARG-DAG: Decl[LocalVar]/Local: weakSelf[#CompleteWeak?#]{{; name=.+$}}
  }
}
