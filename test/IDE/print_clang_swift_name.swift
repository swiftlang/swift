// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -source-filename %s -module-to-print=SwiftNameTests -function-definitions=false -print-regular-comments -F %S/Inputs/mock-sdk > %t.txt
// RUN: diff -u <(tail +9 %s) %t.txt

// REQUIRES: objc_interop

// EXPECTED OUTPUT STARTS BELOW THIS LINE.
@_exported import Foundation


class Test : NSObject {
  
  // "Factory methods" that we'd rather have as initializers.
  @available(*, unavailable, message: "superseded by import of -[NSObject init]")
  convenience init()
  @available(*, unavailable, message: "use object construction 'Test()'")
  @discardableResult
  class func a() -> Self
  convenience init(dummyParam: ())
  @available(*, unavailable, message: "use object construction 'Test(dummyParam:)'")
  @discardableResult
  class func b() -> Self
  
  convenience init(cc x: AnyObject)
  @available(*, unavailable, message: "use object construction 'Test(cc:)'")
  @discardableResult
  class func c(_ x: AnyObject) -> Self
  convenience init(_ x: AnyObject)
  @available(*, unavailable, message: "use object construction 'Test(_:)'")
  @discardableResult
  class func d(_ x: AnyObject) -> Self
  
  convenience init(aa a: AnyObject, _ b: AnyObject, cc c: AnyObject)
  @available(*, unavailable, message: "use object construction 'Test(aa:_:cc:)'")
  @discardableResult
  class func e(_ a: AnyObject, e b: AnyObject, e c: AnyObject) -> Self
  
  /*not inherited*/ init(fixedType: ())
  @available(*, unavailable, message: "use object construction 'Test(fixedType:)'")
  @discardableResult
  class func f() -> Test
  
  // Would-be initializers.
  @discardableResult
  class func zz() -> Self
  @discardableResult
  class func yy(aa x: AnyObject) -> Self
  @discardableResult
  class func xx(_ x: AnyObject, bb xx: AnyObject) -> Self
  
  init()
}

class TestError : NSObject {
  // Factory methods with NSError.
  convenience init(error: ()) throws
  @available(*, unavailable, message: "use object construction 'TestError(error:)'")
  @discardableResult
  class func err1() throws -> Self
  convenience init(aa x: AnyObject?, error: ()) throws
  @available(*, unavailable, message: "use object construction 'TestError(aa:error:)'")
  @discardableResult
  class func err2(_ x: AnyObject?) throws -> Self
  convenience init(aa x: AnyObject?, error: (), block: () -> Void) throws
  @available(*, unavailable, message: "use object construction 'TestError(aa:error:block:)'")
  @discardableResult
  class func err3(_ x: AnyObject?, callback block: () -> Void) throws -> Self
  convenience init(error: (), block: () -> Void) throws
  @available(*, unavailable, message: "use object construction 'TestError(error:block:)'")
  @discardableResult
  class func err4(callback block: () -> Void) throws -> Self
  
  convenience init(aa x: AnyObject?) throws
  @available(*, unavailable, message: "use object construction 'TestError(aa:)'")
  @discardableResult
  class func err5(_ x: AnyObject?) throws -> Self
  convenience init(aa x: AnyObject?, block: () -> Void) throws
  @available(*, unavailable, message: "use object construction 'TestError(aa:block:)'")
  @discardableResult
  class func err6(_ x: AnyObject?, callback block: () -> Void) throws -> Self
  convenience init(block: () -> Void) throws
  @available(*, unavailable, message: "use object construction 'TestError(block:)'")
  @discardableResult
  class func err7(callback block: () -> Void) throws -> Self
  
  // Would-be initializers.
  @discardableResult
  class func ww(_ x: AnyObject?) throws -> Self
  @discardableResult
  class func w2(_ x: AnyObject?, error: ()) throws -> Self
  @discardableResult
  class func vv() throws -> Self
  @discardableResult
  class func v2(error: ()) throws -> Self
  init()
}

class TestSub : Test {
  @available(*, unavailable, message: "superseded by import of -[NSObject init]")
  convenience init()
  convenience init(dummyParam: ())
  convenience init(cc x: AnyObject)
  convenience init(_ x: AnyObject)
  convenience init(aa a: AnyObject, _ b: AnyObject, cc c: AnyObject)
  init()
}

class TestErrorSub : TestError {
  convenience init(error: ()) throws
  convenience init(aa x: AnyObject?, error: ()) throws
  convenience init(aa x: AnyObject?, error: (), block: () -> Void) throws
  convenience init(error: (), block: () -> Void) throws
  convenience init(aa x: AnyObject?) throws
  convenience init(aa x: AnyObject?, block: () -> Void) throws
  convenience init(block: () -> Void) throws
  init()
}
