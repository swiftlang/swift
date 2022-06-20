// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE | %FileCheck %s

struct Foo {
  var x: Int = {
    guard let foo = #^COMPLETE^#
  }()
}

// CHECK: Decl[Struct]/CurrModule:            Foo[#Foo#];
