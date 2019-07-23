import func SomeModule . /*import*/someFunc

func /*no-args:def*/aFunc() -> Int {
    return 1
}

func /*param-label:def*/aFunc(a: Int) {}

func /*arg-label:def*/aFunc(b a:Int) {}

func /*no-label:def*/aFunc(_ b:Int) -> Int {
    return /*no-args:call*/aFunc()
}

func /*whitespace-labels:def*/aFunc(     a  b: Int ,_   a: Int, c c   : Int) {}

func /*referenced:def*/bar(a: Int) {}

func /*varargs:def*/aFunc(c: Int...) {}
/*varargs:call*/aFunc(c: 1, 2, 3, 4)

class AStruct {
    func /*method:def*/foo(a: Int, b: Int, _ c: Int) -> Int {
        return a + b + c
    }

    func /*bar:def*/bar(_ a: Int) -> (Int) -> Int {
        return {a in a};
    }

    static func /*infix-operator:def*/+ (left: AStruct, right: AStruct) -> AStruct {
        return AStruct()
    }

    static prefix func /*prefix-operator:def*/- (struct: AStruct) -> AStruct {
        return AStruct()
    }
}

let aStruct = /*prefix-operator:call*/-AStruct() /*infix-operator:call*/+ AStruct()
/*no-args:call*/aFunc()
/*param-label:call*/aFunc(a: 2)
/*arg-label:call*/aFunc(b: /*no-args:call*/aFunc() * /*no-args:call*/aFunc())
let _ = /*no-label:call*/aFunc(3)
/*whitespace-labels:call*/aFunc( a  :  2 ,2,  c:  4 )

let _ = aStruct . /*method:call*/foo(a: 2, b: 3, 1)
let _ = AStruct . /*method*/foo(aStruct)(a: 1, b: 8, 10)
let _ = aStruct . /*bar:call*/bar(/*no-args:call*/aFunc())(/*no-label:call*/aFunc(2))

var a = /*referenced*/bar
var b = /*referenced*/bar(a:)
let _ = "Some text \(/*param-label:call*/aFunc(a:1)) around"

class SomeClass {
    init() {}
    /*init:def*/init(a: Int, b:Int, c:Int) {}
    /*sub:def*/subscript(x: Int, y j: Int) -> Int {
        get { return 1 }
        set {}
    }
}

let someClass = SomeClass();
let _ = /*init:call*/SomeClass(a:1, b:1, c:1)
let _ = SomeClass . /*init*/init(a:b:c:)
_ = someClass/*sub:ref*/[1, y: 2]
someClass/*sub:ref*/[1, y: 2] = 2

class AnotherClass {
    let bar = AnotherClass()
    func /*nested:def*/foo(a: Int) -> AnotherClass {}
}

AnotherClass() . /*nested:call*/foo(a: 1) . /*nested2*/bar . /*nested2*/bar . /*nested:call*/foo(a: 2) . /*nested:call*/foo(a: 3) . /*nested:unknown*/foo . foo(a: 4)

struct Memberwise {
    let /*memberwise-x:def*/x: Int
    let y: Int = 0
    var z: Int = 2
}
_ = Memberwise(/*memberwise-x:ref*/x: 1, z: 3)
let memberwise = Memberwise.init(/*memberwise-x:ref*/x:z:)
_ = memberwise . /*memberwise-x:ref*/x

// RUN: %empty-directory(%t.result)
// RUN: %refactor -syntactic-rename -source-filename %s -pos="bar" -is-function-like -old-name "bar(_:)" -new-name "barb(first:)" >> %t.result/functions_bar.swift
// RUN: diff -u %S/Outputs/functions/bar.swift.expected %t.result/functions_bar.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="no-args" -is-function-like -old-name "aFunc" -new-name "anotherFunc" >> %t.result/functions_no-args.swift
// RUN: diff -u %S/Outputs/functions/no-args.swift.expected %t.result/functions_no-args.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="param-label" -is-function-like -old-name "aFunc(a:)" -new-name "anotherFunc(param:)" >> %t.result/functions_param-label.swift
// RUN: diff -u %S/Outputs/functions/param-label.swift.expected %t.result/functions_param-label.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="param-label" -is-function-like -old-name "aFunc(a:)" -new-name "aFunc(_:)" >> %t.result/functions_param-label_remove.swift
// RUN: diff -u %S/Outputs/functions/param-label_remove.swift.expected %t.result/functions_param-label_remove.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="arg-label" -is-function-like -old-name "aFunc(b:)" -new-name "anotherFunc(c:)" >> %t.result/functions_arg-label.swift
// RUN: diff -u %S/Outputs/functions/arg-label.swift.expected %t.result/functions_arg-label.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="arg-label" -is-function-like -old-name "aFunc(b:)" -new-name "aFunc(a:)" >> %t.result/functions_arg-label_same.swift
// RUN: diff -u %S/Outputs/functions/arg-label_same.swift.expected %t.result/functions_arg-label_same.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="arg-label" -is-function-like -old-name "aFunc(b:)" -new-name "aFunc(_:)" >> %t.result/functions_arg-label_remove.swift
// RUN: diff -u %S/Outputs/functions/arg-label_remove.swift.expected %t.result/functions_arg-label_remove.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="no-label" -is-function-like -old-name "aFunc(_:)" -new-name "aFunc2(aLabel:)" >> %t.result/functions_no-label.swift
// RUN: diff -u %S/Outputs/functions/no-label.swift.expected %t.result/functions_no-label.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="no-label" -is-function-like -old-name "aFunc(_:)" -new-name "aFunc(b:)" >> %t.result/functions_no-label_same.swift
// RUN: diff -u %S/Outputs/functions/no-label_same.swift.expected %t.result/functions_no-label_same.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="whitespace-labels" -is-function-like -old-name "aFunc(a:_:c:)" -new-name "aFunc(c:b:d:)" >> %t.result/functions_whitespace-labels.swift
// RUN: diff -u %S/Outputs/functions/whitespace-labels.swift.expected %t.result/functions_whitespace-labels.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="referenced" -is-function-like -old-name "bar(a:)" -new-name "bah(b:)" >> %t.result/functions_referenced.swift
// RUN: diff -u %S/Outputs/functions/referenced.swift.expected %t.result/functions_referenced.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="method" -is-function-like -old-name "foo(a:b:_:)" -new-name "myMethod(_:second:do:)" >> %t.result/functions_method.swift
// RUN: diff -u %S/Outputs/functions/method.swift.expected %t.result/functions_method.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="infix-operator" -is-function-like -old-name "+(left:right:)" -new-name "-(left:right:)" >> %t.result/functions_infix-operator.swift
// RUN: diff -u %S/Outputs/functions/infix-operator.swift.expected %t.result/functions_infix-operator.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="prefix-operator" -is-function-like -old-name "-(struct:)" -new-name "+(theStruct:)" >> %t.result/functions_prefix-operator.swift
// RUN: diff -u %S/Outputs/functions/prefix-operator.swift.expected %t.result/functions_prefix-operator.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="init" -is-function-like -old-name "init(a:b:c:)" -new-name "init(_:d:e:)" >> %t.result/functions_init.swift
// RUN: diff -u %S/Outputs/functions/init.swift.expected %t.result/functions_init.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="varargs" -is-function-like -old-name "aFunc(c:)" -new-name "aFunc(_:)" >> %t.result/functions_varargs.swift
// RUN: diff -u %S/Outputs/functions/varargs.swift.expected %t.result/functions_varargs.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="import" -is-function-like -old-name "someFunc(a:)" -new-name "otherFunc(_:)" >> %t.result/functions_import.swift
// RUN: diff -u %S/Outputs/functions/import.swift.expected %t.result/functions_import.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="nested" -is-function-like -old-name "foo(a:)" -new-name "bar(b:)" >> %t.result/functions_nested.swift
// RUN: diff -u %S/Outputs/functions/nested.swift.expected %t.result/functions_nested.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="sub" -is-function-like -old-name "subscript(_:y:)" -new-name "subscript(x:j:)" >> %t.result/functions_sub.swift
// RUN: diff -u %S/Outputs/functions/sub.swift.expected %t.result/functions_sub.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="memberwise-x" -old-name "x" -new-name "new_x" >> %t.result/functions_memberwise-x.swift
// RUN: diff -u %S/Outputs/functions/memberwise-x.swift.expected %t.result/functions_memberwise-x.swift
// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="bar" -is-function-like -old-name "bar(_:)" >> %t.ranges/functions_bar.swift
// RUN: diff -u %S/FindRangeOutputs/functions/bar.swift.expected %t.ranges/functions_bar.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="no-args" -is-function-like -old-name "aFunc" >> %t.ranges/functions_no-args.swift
// RUN: diff -u %S/FindRangeOutputs/functions/no-args.swift.expected %t.ranges/functions_no-args.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="param-label" -is-function-like -old-name "aFunc(a:)" >> %t.ranges/functions_param-label.swift
// RUN: diff -u %S/FindRangeOutputs/functions/param-label.swift.expected %t.ranges/functions_param-label.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="arg-label" -is-function-like -old-name "aFunc(b:)" >> %t.ranges/functions_arg-label.swift
// RUN: diff -u %S/FindRangeOutputs/functions/arg-label.swift.expected %t.ranges/functions_arg-label.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="whitespace-labels" -is-function-like -old-name "aFunc(a:_:c:)" >> %t.ranges/functions_whitespace-labels.swift
// RUN: diff -u %S/FindRangeOutputs/functions/whitespace-labels.swift.expected %t.ranges/functions_whitespace-labels.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="referenced" -is-function-like -old-name "bar(a:)" >> %t.ranges/functions_referenced.swift
// RUN: diff -u %S/FindRangeOutputs/functions/referenced.swift.expected %t.ranges/functions_referenced.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="method" -is-function-like -old-name "foo(a:b:_:)" >> %t.ranges/functions_method.swift
// RUN: diff -u %S/FindRangeOutputs/functions/method.swift.expected %t.ranges/functions_method.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="infix-operator" -is-function-like -old-name "+(left:right:)" >> %t.ranges/functions_infix-operator.swift
// RUN: diff -u %S/FindRangeOutputs/functions/infix-operator.swift.expected %t.ranges/functions_infix-operator.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="prefix-operator" -is-function-like -old-name "-(struct:)" >> %t.ranges/functions_prefix-operator.swift
// RUN: diff -u %S/FindRangeOutputs/functions/prefix-operator.swift.expected %t.ranges/functions_prefix-operator.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="init" -is-function-like -old-name "init(a:b:c:)" >> %t.ranges/functions_init.swift
// RUN: diff -u %S/FindRangeOutputs/functions/init.swift.expected %t.ranges/functions_init.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="sub" -is-function-like -old-name "subscript(_:y:)" >> %t.ranges/functions_sub.swift
// RUN: diff -u %S/FindRangeOutputs/functions/sub.swift.expected %t.ranges/functions_sub.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="memberwise-x" -old-name "x" >> %t.ranges/functions_memberwise-x.swift
// RUN: diff -u %S/FindRangeOutputs/functions/memberwise-x.swift.expected %t.ranges/functions_memberwise-x.swift
