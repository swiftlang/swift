// RUN: not %swift -emit-sil -target %target-triple %s -emit-fixits-path %t.remap -I %S/Inputs -diagnostics-editor-mode
// RUN: c-arcmt-test %t.remap | arcmt-test -verify-transformed-files %s.result

func something() -> Bool { return true }

func foo1() {
    if something() {
    } else
}

func foo2() {
    if something() {
    } else
    foo1()
}

func foo3() {
    if something() {
    } else something() { // all on one line, 'if' inserted
    }
}

func foo4() {
    if something() {
    } else something()
    {
    }
}

func foo5() {
    if something() {
    } else something()
    foo1()
}
