// RUN: %batch-code-completion

struct Foo {
  func bar(argLabel: Int) -> Int { 1 }
}
let x = Foo().bar
x.#^AFTER_DOT^#
// AFTER_DOT: Pattern/CurrModule/Flair[ArgLabels]/Erase[1]: ({#Int#})[#Int#]; name=() 

x. #^AFTER_DOT_AND_SPACE^#
// AFTER_DOT_AND_SPACE: Pattern/CurrModule/Flair[ArgLabels]/Erase[2]: ({#Int#})[#Int#]; name=() 

x.#^AFTER_DOT_FOLLOWING_DOT?check=AFTER_DOT^#.

Foo().bar.#^UNRESOLVED_MEMBER_REF^#
// UNRESOLVED_MEMBER_REF: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]/Erase[1]: ({#argLabel: Int#})[#Int#]; name=(argLabel:)
