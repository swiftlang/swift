/*
    /*foo:unknown*/foo() is not /*foo:unknown*/foo(first:)
*/
/// This describes /*foo:unknown*/foo and /*foo:unknown*/foo
func /*foo:def*/foo() {
    let /*foo:def*/foo = "Here is /*foo:unknown*/foo"
    // /*foo:unknown*/foo's return
    #selector(Struct . /*foo:unknown*/foo(_:aboveSubview:))
    #selector(/*foo:unknown*/foo(_:))
    #selector(#selector(/*foo:unknown*/foo))

    #if true
        /*foo*/foo = 2
        /*foo*/foo()
        /*foo:call*/foo()
        /*foo:unknown*/foo = 3
        /*foo:unknown*/foo()
        #if false
            /*foo:unknown*/foo += 2
            /*foo:unknown*/foo()
        #endif
    #else
        /*foo:unknown*/foo = 4
    #endif

    return 1
}

#if false
class /*MyClass:unknown*/MyClass {}
_ = /*MyClass:unknown*/Mismatch()
_ = /*MyClass:unknown*/MyClass()
#else
class /*MyClass:unknown*/MyClass {}
_ = /*MyClass:unknown*/Mismatch()
_ = /*MyClass:unknown*/MyClass()
#endif

// RUN: %empty-directory(%t.result)
// RUN: %refactor -syntactic-rename -source-filename %s -pos="foo" -is-function-like -old-name "foo" -new-name "bar" >> %t.result/textual_foo.swift
// RUN: diff -u %S/Outputs/textual/foo.swift.expected %t.result/textual_foo.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="MyClass" -is-non-protocol-type -old-name "MyClass" -new-name "YourClass" >> %t.result/textual_MyClass.swift
// RUN: diff -u %S/Outputs/textual/MyClass.swift.expected %t.result/textual_MyClass.swift
// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="foo" -is-function-like -old-name "foo" >> %t.ranges/textual_foo.swift
// RUN: diff -u %S/FindRangeOutputs/textual/foo.swift.expected %t.ranges/textual_foo.swift
