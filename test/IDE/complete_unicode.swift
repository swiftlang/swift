// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=UNICODE_1 > %t.unicode.txt
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
// UNICODE_1: Begin completions, 8 items
// UNICODE_1-NEXT: Decl/CurrNominal: Идентификаторы_с_кириллицей_допустимы()[#Void#]{{$}}
// UNICODE_1-NEXT: Decl/CurrNominal: Ідентіфікатори_українською_також_працюють()[#Void#]{{$}}
// UNICODE_1-NEXT: Decl/CurrNominal: 識別子は()[#Void#]{{$}}
// UNICODE_1-NEXT: Decl/CurrNominal: ひらがなでも()[#Void#]{{$}}
// UNICODE_1-NEXT: Decl/CurrNominal: カタカナでも()[#Void#]{{$}}
// UNICODE_1-NEXT: Decl/CurrNominal: 漢字でも()[#Void#]{{$}}
// UNICODE_1-NEXT: Decl/CurrNominal: いいです()[#Void#]{{$}}
// UNICODE_1-NEXT: Keyword/None:     metatype[#Unicode1.metatype#]{{$}}
// UNICODE_1-NEXT: End completions

