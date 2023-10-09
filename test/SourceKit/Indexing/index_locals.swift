// RUN: %sourcekitd-test -req=index -req-opts=should_index_locals=1 %s -- -Xfrontend -serialize-diagnostics-path -Xfrontend %t.dia -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import %s | %sed_clean > %t.response
// RUN: %diff -u %s.response %t.response

func foo(a: Int, b: Double) {
    var locVar = 1
}

var globalVar = 2

class A : ExpressibleByIntegerLiteral {
    required init(integerLiteral value: IntegerLiteralType) {}
}

func bar() {
    var locVar1: A = 1
    let locVar2 = 2
    foo(a: globalVar, b: Double(locVar2))
    let closure = { (arg1: Int, arg2: Int) -> Int in
         let locVar3 = 3
         let expr: Int = locVar2 + globalVar + arg1
         return expr
    }
}

class X {
    func baz() {
        func inner(_ x: Int) -> Int { return x }
        var arr: [Int] = [1, 2, 3]
        if (globalVar > 2) {
            arr = arr.map { inner($0) }
        }
    }
}
