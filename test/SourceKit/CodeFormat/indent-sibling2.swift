func foo(foo: Int, bar: Int, baz: Int, buzz: Int) -> Int {
    return foo + bar + baz + buzz
}

foo(0,
  bar: 1,
      baz: 2,
          buzz: 3)

public enum ProtobufJSONToken: Equatable {
  case COLON
  case COMMA
  case BEGIN_OBJECT
}

public func ==(lhs: ProtobufJSONToken, rhs: ProtobufJSONToken) -> Bool {
  switch (lhs, rhs) {
  case (.COLON, .COLON),
          (.COMMA, .COMMA),
              (.BEGIN_OBJECT, .BEGIN_OBJECT),
    return true
  default:
    return false
  }
}

// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=19 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=20 -length=1 %s >>%t.response
// RUN: FileCheck --strict-whitespace %s <%t.response

//                        "foo(0,"
// CHECK: key.sourcetext: "    bar: 1,"

//                        "  bar: 1,"
// CHECK: key.sourcetext: "  baz: 2,"

//                        "      baz: 2,"
// CHECK: key.sourcetext: "      buzz: 3)"

//                        "  case (.COLON, .COLON),"
// CHECK: key.sourcetext: "       (.COMMA, .COMMA),"
//                        "          (.COMMA, .COMMA),"
// CHECK: key.sourcetext: "          (.BEGIN_OBJECT, .BEGIN_OBJECT),"
