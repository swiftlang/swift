// RUN: %batch-code-completion

struct Bar() {
    init?(withInout: inout Int) {}
    init?(withPointer: UnsafePointer<Int>) {}
}

struct Foo {
    var myInt: Int

    func bar() {
        let context = Bar(withInout: &self.#^COMPLETE_INOUT?check=CHECK^#)
        let context = Bar(withPointer: &self.#^COMPLETE_POINTER?check=CHECK^#)
    }
}

// CHECK: Decl[InstanceVar]/CurrNominal:      myInt[#Int#]; name=myInt