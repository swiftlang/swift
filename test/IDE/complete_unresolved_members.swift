// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_1 | FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_2 | FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_3 | FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_4 | FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_5 | FileCheck %s -check-prefix=UNRESOLVED_1

enum SomeEnum1 {
  case South
  case North
}

struct NotOptions1 {
  static let NotSet = 1;
}

struct SomeOptions1 : OptionSetType {
  let rawValue : Int
  static let Option1 = SomeOptions1(rawValue: 1 << 1)
  static let Option2 = SomeOptions1(rawValue: 1 << 2)
  static let Option3 = SomeOptions1(rawValue: 1 << 3)
  let NotStaticOption = (rawValue: 1 << 4)
}

func OptionSetTaker1(Op : SomeOptions1) {}

class C1 {
  func f1() {
    var f = .#^UNRESOLVED_1^#
  }
  func f2() {
    var f = [.#^UNRESOLVED_2^#
  }
  func f3() {
    var f = [.SET1, .#^UNRESOLVED_3^#
  }
  func f4() {
    OptionSetTaker1(.#^UNRESOLVED_4^#
  }
  func f5() {
    OptionSetTaker1([.Option1, .#^UNRESOLVED_5^#
  }
// UNRESOLVED_1:  Begin completions
// UNRESOLVED_1-DAG:  Decl[EnumElement]/ExprSpecific:     North[#SomeEnum1#]; name=North
// UNRESOLVED_1-DAG:  Decl[EnumElement]/ExprSpecific:     South[#SomeEnum1#]; name=South
// UNRESOLVED_1-DAG:  Decl[StaticVar]/CurrModule: Option1[#SomeOptions1#]; name=Option1
// UNRESOLVED_1-DAG:  Decl[StaticVar]/CurrModule: Option2[#SomeOptions1#]; name=Option2
// UNRESOLVED_1-DAG:  Decl[StaticVar]/CurrModule: Option3[#SomeOptions1#]; name=Option3
// UNRESOLVED_1-NOT:  NotOptions1
// UNRESOLVED_1-NOT:  NotStaticOption
}
