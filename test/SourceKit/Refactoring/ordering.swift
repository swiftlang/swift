class C {
    func /*C_foo1*/foo1(/*C_foo1_arg*/_ x: Int) {
        print(x)
    }
    func /*C_foo2*/foo2(
        /*C_foo2_arg*/a: Int) {
        print(a)
    }
    func /*C_foo3*/foo3(/*C_foo3_arg1*/a: Int, 
              /*C_foo3_arg2*/b: Int) {
        print(a)
        print(b)
    }
}

class D: C {
    override func /*D_foo1*/foo1(
        /*D_foo1_arg*/_ y: Int) {
        print(y)
    }
    override func /*D_foo2*/foo2(/*D_foo2_arg*/a x: Int) {
        print(x)
    }
    
    override func /*D_foo3*/foo3(/*D_foo3_arg1*/a x: Int, /*D_foo3_arg2*/b y: Int) {
        print(x)
        print(y)
    }
}

func test(c: C) {
    c . /*C_foo1_call*/foo1(/*C_foo1_call_arg*/1)
    c . /*C_foo2_call*/foo2(/*C_foo2_call_arg*/a: 1)
    c . /*C_foo3_call*/foo3(/*C_foo3_call_arg1*/a: 1,
                            /*C_foo3_call_arg2*/b: 2)
}

// RUN: %empty-directory(%t.result)
// RUN: %sourcekitd-test -req=syntactic-rename -rename-spec %S/ordering/ordering.in.json %s >> %t.result/ordering.expected
// RUN: diff -u %S/ordering/ordering.expected %t.result/ordering.expected
