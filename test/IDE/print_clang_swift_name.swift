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
  class func a() -> Self
  convenience init(dummyParam: ())
  @available(*, unavailable, message: "use object construction 'Test(dummyParam:)'")
  class func b() -> Self
  
  convenience init(cc x: Any)
  @available(*, unavailable, message: "use object construction 'Test(cc:)'")
  class func c(_ x: Any) -> Self
  convenience init(_ x: Any)
  @available(*, unavailable, message: "use object construction 'Test(_:)'")
  class func d(_ x: Any) -> Self
  
  convenience init(aa a: Any, _ b: Any, cc c: Any)
  @available(*, unavailable, message: "use object construction 'Test(aa:_:cc:)'")
  class func e(_ a: Any, e b: Any, e c: Any) -> Self
  
  /*not inherited*/ init(fixedType: ())
  @available(*, unavailable, message: "use object construction 'Test(fixedType:)'")
  class func f() -> Test
  
  // Would-be initializers.
  class func zz() -> Self
  class func yy(aa x: Any) -> Self
  class func xx(_ x: Any, bb xx: Any) -> Self
  
  init()
}

class TestError : NSObject {
  // Factory methods with NSError.
  convenience init(error: ()) throws
  @available(*, unavailable, message: "use object construction 'TestError(error:)'")
  class func err1() throws -> Self
  convenience init(aa x: Any?, error: ()) throws
  @available(*, unavailable, message: "use object construction 'TestError(aa:error:)'")
  class func err2(_ x: Any?) throws -> Self
  convenience init(aa x: Any?, error: (), block: @escaping () -> Void) throws
  @available(*, unavailable, message: "use object construction 'TestError(aa:error:block:)'")
  class func err3(_ x: Any?, callback block: @escaping () -> Void) throws -> Self
  convenience init(error: (), block: @escaping () -> Void) throws
  @available(*, unavailable, message: "use object construction 'TestError(error:block:)'")
  class func err4(callback block: @escaping () -> Void) throws -> Self
  
  convenience init(aa x: Any?) throws
  @available(*, unavailable, message: "use object construction 'TestError(aa:)'")
  class func err5(_ x: Any?) throws -> Self
  convenience init(aa x: Any?, block: @escaping () -> Void) throws
  @available(*, unavailable, message: "use object construction 'TestError(aa:block:)'")
  class func err6(_ x: Any?, callback block: @escaping () -> Void) throws -> Self
  convenience init(block: @escaping () -> Void) throws
  @available(*, unavailable, message: "use object construction 'TestError(block:)'")
  class func err7(callback block: @escaping () -> Void) throws -> Self
  
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
