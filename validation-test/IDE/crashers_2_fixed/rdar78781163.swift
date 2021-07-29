// RUN: %swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename %s

class C {
    func foo() {
        #^COMPLETE^#
        default
    }
    func bar() {}
}
