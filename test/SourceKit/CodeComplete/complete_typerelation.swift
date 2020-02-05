protocol MyProto {}
enum MyEnum : MyProto {
    case foo
    case bar(Int)

    static func staticReturnVoid() {}
    static func staticReturnMyEnum() -> MyEnum { return .foo }
    func intanceReturnVoid() {}
    func intanceReturnMyEnum() -> MyEnum { return .foo }
}

func testIdenticalContext() -> MyEnum {
  return MyEnum.
}

func testConvertibleContext() -> MyProto {
  return MyEnum.
}

// RUN: %sourcekitd-test -req=complete -pos=13:17 %s -- %s > %t.identical.response
// RUN: diff -u %s.identical.response %t.identical.response

// RUN: %sourcekitd-test -req=complete -pos=17:17 %s -- %s > %t.convertible.response
// RUN: diff -u %s.convertible.response %t.convertible.response
