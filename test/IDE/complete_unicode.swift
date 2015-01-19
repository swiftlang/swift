// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNICODE_1 > %t.unicode.txt
// RUN: FileCheck %s -check-prefix=UNICODE_1 < %t.unicode.txt

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
// UNICODE_1: Begin completions
// UNICODE_1-NEXT: Decl[InstanceMethod]/CurrNominal: Идентификаторы_с_кириллицей_допустимы()[#Void#]{{$}}
// UNICODE_1-NEXT: Decl[InstanceMethod]/CurrNominal: Ідентіфікатори_українською_також_працюють()[#Void#]{{$}}
// UNICODE_1-NEXT: Decl[InstanceMethod]/CurrNominal: 識別子は()[#Void#]{{$}}
// UNICODE_1-NEXT: Decl[InstanceMethod]/CurrNominal: ひらがなでも()[#Void#]{{$}}
// UNICODE_1-NEXT: Decl[InstanceMethod]/CurrNominal: カタカナでも()[#Void#]{{$}}
// UNICODE_1-NEXT: Decl[InstanceMethod]/CurrNominal: 漢字でも()[#Void#]{{$}}
// UNICODE_1-NEXT: Decl[InstanceMethod]/CurrNominal: いいです()[#Void#]{{$}}
// UNICODE_1-NEXT: End completions

