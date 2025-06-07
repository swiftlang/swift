// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -source-filename %s -module-to-print=SwiftNameTests -function-definitions=false -F %S/Inputs/mock-sdk > %t.txt
// RUN: diff -u <(tail -n +9 %s) %t.txt

// REQUIRES: objc_interop

// EXPECTED OUTPUT STARTS BELOW THIS LINE.
@_exported import Foundation

class Test : NSObject {
  @available(*, unavailable, message: "superseded by import of -[NSObject init]")
  convenience init()
  @available(*, unavailable, renamed: "init()", message: "Not available in Swift")
  class func a() -> Self
  convenience init(dummyParam: ())
  @available(*, unavailable, renamed: "init(dummyParam:)", message: "Not available in Swift")
  class func b() -> Self
  convenience init(cc x: Any)
  @available(*, unavailable, renamed: "init(cc:)", message: "Not available in Swift")
  class func c(_ x: Any) -> Self
  convenience init(_ x: Any)
  @available(*, unavailable, renamed: "init(_:)", message: "Not available in Swift")
  class func d(_ x: Any) -> Self
  convenience init(aa a: Any, _ b: Any, cc c: Any)
  @available(*, unavailable, renamed: "init(aa:_:cc:)", message: "Not available in Swift")
  class func e(_ a: Any, e b: Any, e c: Any) -> Self
  /*not inherited*/ init(fixedType: ())
  @available(*, unavailable, renamed: "init(fixedType:)", message: "Not available in Swift")
  class func f() -> Test
  class func zz() -> Self
  @available(swift, obsoleted: 3, renamed: "zz()")
  class func testZ() -> Self
  class func yy(aa x: Any) -> Self
  @available(*, unavailable, renamed: "yy(aa:)", message: "Not available in Swift")
  class func testY(_ x: Any) -> Self
  class func xx(_ x: Any, bb xx: Any) -> Self
  @available(*, unavailable, renamed: "xx(_:bb:)", message: "Not available in Swift")
  class func testX(_ x: Any, xx: Any) -> Self
  init()
}
class TestError : NSObject {
  convenience init(error: ()) throws
  @available(*, unavailable, renamed: "init(error:)", message: "Not available in Swift")
  class func err1() throws -> Self
  convenience init(aa x: Any?, error: ()) throws
  @available(*, unavailable, renamed: "init(aa:error:)", message: "Not available in Swift")
  class func err2(_ x: Any?) throws -> Self
  convenience init(aa x: Any?, error: (), block: @escaping () -> Void) throws
  @available(*, unavailable, renamed: "init(aa:error:block:)", message: "Not available in Swift")
  class func err3(_ x: Any?, callback block: @escaping () -> Void) throws -> Self
  convenience init(error: (), block: @escaping () -> Void) throws
  @available(*, unavailable, renamed: "init(error:block:)", message: "Not available in Swift")
  class func err4(callback block: @escaping () -> Void) throws -> Self
  convenience init(aa x: Any?) throws
  @available(*, unavailable, renamed: "init(aa:)", message: "Not available in Swift")
  class func err5(_ x: Any?) throws -> Self
  convenience init(aa x: Any?, block: @escaping () -> Void) throws
  @available(*, unavailable, renamed: "init(aa:block:)", message: "Not available in Swift")
  class func err6(_ x: Any?, callback block: @escaping () -> Void) throws -> Self
  convenience init(block: @escaping () -> Void) throws
  @available(*, unavailable, renamed: "init(block:)", message: "Not available in Swift")
  class func err7(callback block: @escaping () -> Void) throws -> Self
  class func ww(_ x: Any?) throws -> Self
  @available(swift, obsoleted: 3, renamed: "ww(_:)")
  class func testW(_ x: Any?) throws -> Self
  class func w2(_ x: Any?, error: ()) throws -> Self
  @available(swift, obsoleted: 3, renamed: "w2(_:error:)")
  class func testW2(_ x: Any?) throws -> Self
  class func vv() throws -> Self
  @available(swift, obsoleted: 3, renamed: "vv()")
  class func testV() throws -> Self
  class func v2(error: ()) throws -> Self
  @available(swift, obsoleted: 3, renamed: "v2(error:)")
  class func testV2() throws -> Self
  init()
}
class TestSub : Test {
  @available(*, unavailable, message: "superseded by import of -[NSObject init]")
  convenience init()
  convenience init(dummyParam: ())
  convenience init(cc x: Any)
  convenience init(_ x: Any)
  convenience init(aa a: Any, _ b: Any, cc c: Any)
  init()
}
class TestErrorSub : TestError {
  convenience init(error: ()) throws
  convenience init(aa x: Any?, error: ()) throws
  convenience init(aa x: Any?, error: (), block: @escaping () -> Void) throws
  convenience init(error: (), block: @escaping () -> Void) throws
  convenience init(aa x: Any?) throws
  convenience init(aa x: Any?, block: @escaping () -> Void) throws
  convenience init(block: @escaping () -> Void) throws
  init()
}
