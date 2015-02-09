// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=WEAK_VARS_1 | FileCheck %s -check-prefix=WEAK_VARS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=WEAK_NO_DOT_1 | FileCheck %s -check-prefix=WEAK_NO_DOT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=WEAK_DOT_1 | FileCheck %s -check-prefix=WEAK_DOT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNOWNED_NO_DOT_1 | FileCheck %s -check-prefix=UNOWNED_NO_DOT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNOWNED_DOT_1 | FileCheck %s -check-prefix=UNOWNED_DOT_1

class CompleteWeak {
  func instanceFunc() {}

  func weakVars() {
    weak var weakSelf = self
    unowned var unownedSelf = self
    #^WEAK_VARS_1^#
  }
// WEAK_VARS_1: Begin completions
// WEAK_VARS_1-DAG: Decl[LocalVar]/Local: weakSelf[#CompleteWeak?#]{{; name=.+$}}
// WEAK_VARS_1-DAG: Decl[LocalVar]/Local: unownedSelf[#CompleteWeak#]{{; name=.+$}}
// WEAK_VARS_1: End completions

  func weakNoDot() {
    weak var weakSelf = self
    weakSelf#^WEAK_NO_DOT_1^#
  }
// WEAK_NO_DOT_1: Begin completions
// WEAK_NO_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: ?.instanceFunc()[#Void#]{{; name=.+$}}
// WEAK_NO_DOT_1: End completions

  func weakDot() {
    weak var weakSelf = self
    weakSelf.#^WEAK_DOT_1^#
  }
// WEAK_DOT_1: Begin completions
// WEAK_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal/Erase[1]: ?.instanceFunc()[#Void#]{{; name=.+$}}
// WEAK_DOT_1: End completions

  func unownedNoDot() {
    unowned var unownedSelf = self
    unownedSelf#^UNOWNED_NO_DOT_1^#
  }
// UNOWNED_NO_DOT_1: Begin completions
// UNOWNED_NO_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: .instanceFunc()[#Void#]{{; name=.+$}}
// UNOWNED_NO_DOT_1: End completions

  func unownedDot() {
    unowned var unownedSelf = self
    unownedSelf.#^UNOWNED_DOT_1^#
  }
// UNOWNED_DOT_1: Begin completions
// UNOWNED_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc()[#Void#]{{; name=.+$}}
// UNOWNED_DOT_1: End completions

}
