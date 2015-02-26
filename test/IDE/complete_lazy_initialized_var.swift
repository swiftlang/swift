// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LAZY_IN_CLASS_1 | FileCheck %s -check-prefix=LAZYVAR1
class FooClass1 {
 lazy var lazyVar1 = 0
}

func lazyInClass1(a: FooClass1) {
 a.#^LAZY_IN_CLASS_1^#
}

// This test checks that we don’t include extra hidden declarations into code completion results.  If you add more declarations to the type, update this test properly.
// LAZYVAR1:       Begin completions, 1 items
// LAZYVAR1-NEXT:  Decl[InstanceVar]/CurrNominal:      lazyVar1[#Int#]{{; name=.+$}}
// LAZYVAR1-NEXT:  End completions
