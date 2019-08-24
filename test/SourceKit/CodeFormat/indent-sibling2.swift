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

func f(a : Int,
        bb b : Int,
      cc c :Int) -> Int {
  return 1
}

static func _consoleConnected(type: CGSNotificationType,
        _ data: CGSNotificationData,
            _ length: CGSByteCount,
        _ arg: CGSNotificationArg,
            _ cid: CGSConnectionID) {
}

public func someTestFunc(withArgumentLabel label: String,
              someOtherArgumentLabel label2: String,
                      andAnotherArgumentLabel label3: String) {
}

// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=19 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=20 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=29 -length=1 %s >>%t.response

// RUN: %sourcekitd-test -req=format -line=34 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=35 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=36 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=37 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=41 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=42 -length=1 %s >>%t.response

// RUN: %FileCheck --strict-whitespace %s <%t.response

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

//                        "        bb b : Int,"
// CHECK: key.sourcetext: "        cc c :Int) -> Int {"

//                        "static func _consoleConnected(type: CGSNotificationType,"
// CHECK: key.sourcetext: "                              _ data: CGSNotificationData,"

//                        "        _ data: CGSNotificationData,"
// CHECK: key.sourcetext: "        _ length: CGSByteCount,"

//                        "            _ length: CGSByteCount,"
// CHECK: key.sourcetext: "            _ arg: CGSNotificationArg,"

//                        "        _ arg: CGSNotificationArg,"
// CHECK: key.sourcetext: "        _ cid: CGSConnectionID) {"

//                        "public func someTestFunc(withArgumentLabel label: String,"
// CHECK: key.sourcetext: "                         someOtherArgumentLabel label2: String,"

//                        "              someOtherArgumentLabel label2: String,"
// CHECK: key.sourcetext: "              andAnotherArgumentLabel label3: String) {"
