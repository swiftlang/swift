// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

struct Bar() {
    init?(withInout: inout Int) {}
    init?(withPointer: UnsafePointer<Int>) {}
}

struct Foo {
    var myInt: Int

    func bar() {
        let context = Bar(wihtInout: &self.#^COMPLETE_INOUT?check=CHECK^#)
        let context = Bar(withPointer: &self.#^COMPLETE_POINTER?check=CHECK^#)
    }
}

// CHECK: Decl[InstanceVar]/CurrNominal:      myInt[#Int#]; name=myInt