// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNICODE_1 > %t.unicode.txt
// RUN: %FileCheck %s -check-prefix=UNICODE_1 < %t.unicode.txt

struct Unicode1 {
  func Идентификаторы_с_кириллицей_допустимы() {}
  func Ідентіфікатори_українською_також_працюють() {}
  func 識別子は() {}
  func ひらがなでも() {}
  func カタカナでも() {}
  func 漢字でも() {}
  func いいです() {}
}

func unicode_test_1() {
  Unicode1().#^UNICODE_1^#
}
// UNICODE_1: Begin completions, 8 items
// UNICODE_1-DAG: Keyword[self]/CurrNominal: self[#Unicode1#]; name=self
// UNICODE_1-DAG: Decl[InstanceMethod]/CurrNominal: Идентификаторы_с_кириллицей_допустимы()[#Void#]{{; name=.+$}}
// UNICODE_1-DAG: Decl[InstanceMethod]/CurrNominal: Ідентіфікатори_українською_також_працюють()[#Void#]{{; name=.+$}}
// UNICODE_1-DAG: Decl[InstanceMethod]/CurrNominal: 識別子は()[#Void#]{{; name=.+$}}
// UNICODE_1-DAG: Decl[InstanceMethod]/CurrNominal: ひらがなでも()[#Void#]{{; name=.+$}}
// UNICODE_1-DAG: Decl[InstanceMethod]/CurrNominal: カタカナでも()[#Void#]{{; name=.+$}}
// UNICODE_1-DAG: Decl[InstanceMethod]/CurrNominal: 漢字でも()[#Void#]{{; name=.+$}}
// UNICODE_1-DAG: Decl[InstanceMethod]/CurrNominal: いいです()[#Void#]{{; name=.+$}}
