func foo() {
    bar() {
        var abc = 1
        let a: String = {
          let b = "asdf"
          return b
        }()
    }
}

class C {
  private static let durationTimeFormatter: NSDateComponentsFormatter = {
		return timeFormatter
}()
}

func foo1(a: Int, handler : () -> ()) {}
func foo2(handler : () -> ()) {}

func foo3() {
  foo1(1)
    {
  }
}

func foo4() {
    test = {
        return 0
    }()
    let test = {
        return 0
    }()
}

func foo5(input: Int, block: (Int) -> ()) -> Int {
  return 0
}

func foo6() {
  _ = foo5(input: 0, block: { [unowned self] blockInput in
    foo4()
  })
}

func foo7(A: ()->(), B: ()->()) {}

func foo8() {
  foo7(A: { _ in
    print("hello")
}, B: {
    print("world")
  })
}

func foo9(input: [Int]){
    input.map { (ele) in
        ele + 1
    }.filter{(ele) in
        return ele > 10
    }.map {(ele) in
        return ele + 1
    }
}

func foo10() {
    Something() [
    ].whatever
}

func foo11() {
    VStack {
    }
        .onAppear {
    }
}

func foo12() {
    VStack {
    }
        .onAppear1()
        .onAppear2() {}
        .onAppear3() {
        }
        .onAppear4() {}
}

// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=14 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=22 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=27 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=28 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=29 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=30 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=31 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=32 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=42 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=50 -length=1 %s >>%t.response

// RUN: %sourcekitd-test -req=format -line=55 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=56 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=57 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=58 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=59 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=60 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=61 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=62 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=63 -length=1 %s >>%t.response

// RUN: %sourcekitd-test -req=format -line=66 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=67 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=73 -length=1 %s >>%t.response

// RUN: %sourcekitd-test -req=format -line=80 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=81 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=82 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=83 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=84 -length=1 %s >>%t.response

// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "        var abc = 1"
// CHECK: key.sourcetext: "        let a: String = {"
// CHECK: key.sourcetext: "            let b = \"asdf\""
// CHECK: key.sourcetext: "            return b"
// CHECK: key.sourcetext: "        }()"
// CHECK: key.sourcetext: "    }"

//                        "  private static let durationTimeFormatter: NSDateComponentsFormatter = {"
// CHECK: key.sourcetext: "  }()"
//                        "  foo1(1)"
// CHECK: key.sourcetext: "  {"

// CHECK: key.sourcetext: "    test = {"
// CHECK: key.sourcetext: "        return 0"
// CHECK: key.sourcetext: "    }()"


// CHECK: key.sourcetext: "    let test = {"
// CHECK: key.sourcetext: "        return 0"
// CHECK: key.sourcetext: "    }()"

// CHECK: key.sourcetext: "  })"

// CHECK: key.sourcetext: "  }, B: {"

// CHECK: key.sourcetext: "func foo9(input: [Int]){"
// CHECK: key.sourcetext: "    input.map { (ele) in"
// CHECK: key.sourcetext: "        ele + 1"
// CHECK: key.sourcetext: "    }.filter{(ele) in"
// CHECK: key.sourcetext: "        return ele > 10"
// CHECK: key.sourcetext: "    }.map {(ele) in"
// CHECK: key.sourcetext: "        return ele + 1"
// CHECK: key.sourcetext: "    }"
// CHECK: key.sourcetext: "}"

// CHECK: key.sourcetext: "    Something() ["
// CHECK: key.sourcetext: "    ].whatever"
// CHECK: key.sourcetext: "    .onAppear {"

// CHECK: key.sourcetext: "    .onAppear1()"
// CHECK: key.sourcetext: "    .onAppear2() {}"
// CHECK: key.sourcetext: "    .onAppear3() {"
// CHECK: key.sourcetext: "    }"
// CHECK: key.sourcetext: "    .onAppear4() {}"
