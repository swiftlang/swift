struct Foo {
    let x: Int
    let y: String
    func perform(_ action: (Int, Int) -> ()) {}
}

func test() {
    let x = Foo.init(x: 2, y: "hello")
    x.perform {
        print($0 + $1)
    }
}

// RUN: %sourcekitd-test -req=cursor -pos=8:17 %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: <Declaration>init(x: <Type usr="s:Si">Int</Type>, y: <Type usr="s:SS">String</Type>)</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=10:15 %s -- %s | %FileCheck -check-prefix=CHECK2 %s
// CHECK2: <Declaration>let $0: <Type usr="s:Si">Int</Type></Declaration>
