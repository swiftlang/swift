// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -source-filename %s -module-to-print=SwiftNameTests -function-definitions=false -print-regular-comments -F %S/Inputs/mock-sdk > %t.txt
// RUN: diff -u <(tail +9 %s) %t.txt

// REQUIRES: objc_interop

// EXPECTED OUTPUT STARTS BELOW THIS LINE.
@_exported import Foundation


class Test : NSObject {
  
  // "Factory methods" that we'd rather have as initializers.
  @available(*, unavailable, renamed: "init()", message: "Not available in Swift")
  class func a() -> Self
  @available(*, unavailable, message: "superseded by import of -[NSObject init]")
  convenience init()
  @available(*, unavailable, renamed: "init(dummyParam:)", message: "Not available in Swift")
  class func b() -> Self
  convenience init(dummyParam: ())
  
  @available(*, unavailable, renamed: "init(cc:)", message: "Not available in Swift")
  class func c(_ x: Any) -> Self
  convenience init(cc x: Any)
  @available(*, unavailable, renamed: "init(_:)", message: "Not available in Swift")
  class func d(_ x: Any) -> Self
  convenience init(_ x: Any)
  
  @available(*, unavailable, renamed: "init(aa:_:cc:)", message: "Not available in Swift")
  class func e(_ a: Any, e b: Any, e c: Any) -> Self
  convenience init(aa a: Any, _ b: Any, cc c: Any)
  
  @available(*, unavailable, renamed: "init(fixedType:)", message: "Not available in Swift")
  class func f() -> Test
  /*not inherited*/ init(fixedType: ())
  
  // Would-be initializers.
  class func zz() -> Self
  class func yy(aa x: Any) -> Self
  class func xx(_ x: Any, bb xx: Any) -> Self
  
  init()
}

class TestError : NSObject {
  // Factory methods with NSError.
  @available(*, unavailable, renamed: "init(error:)", message: "Not available in Swift")
  class func err1() throws -> Self
  convenience init(error: ()) throws
  @available(*, unavailable, renamed: "init(aa:error:)", message: "Not available in Swift")
  class func err2(_ x: Any?) throws -> Self
  convenience init(aa x: Any?, error: ()) throws
  @available(*, unavailable, renamed: "init(aa:error:block:)", message: "Not available in Swift")
  class func err3(_ x: Any?, callback block: @escaping () -> Void) throws -> Self
  convenience init(aa x: Any?, error: (), block: @escaping () -> Void) throws
  @available(*, unavailable, renamed: "init(error:block:)", message: "Not available in Swift")
  class func err4(callback block: @escaping () -> Void) throws -> Self
  convenience init(error: (), block: @escaping () -> Void) throws
  
  @available(*, unavailable, renamed: "init(aa:)", message: "Not available in Swift")
  class func err5(_ x: Any?) throws -> Self
  convenience init(aa x: Any?) throws
  @available(*, unavailable, renamed: "init(aa:block:)", message: "Not available in Swift")
  class func err6(_ x: Any?, callback block: @escaping () -> Void) throws -> Self
  convenience init(aa x: Any?, block: @escaping () -> Void) throws
  @available(*, unavailable, renamed: "init(block:)", message: "Not available in Swift")
  class func err7(callback block: @escaping () -> Void) throws -> Self
  convenience init(block: @escaping () -> Void) throws
  
  // Would-be initializers.
  class func ww(_ x: Any?) throws -> Self
  class func w2(_ x: Any?, error: ()) throws -> Self
  class func vv() throws -> Self
  class func v2(error: ()) throws -> Self
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
