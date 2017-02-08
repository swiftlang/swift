// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -source-filename %s -module-to-print=SwiftNameTests -function-definitions=false -print-regular-comments -F %S/Inputs/mock-sdk > %t.txt
// RUN: diff -u <(tail +9 %s) %t.txt

// REQUIRES: objc_interop

// EXPECTED OUTPUT STARTS BELOW THIS LINE.
@_exported import Foundation


class Test : NSObject {
  
  @available(*, unavailable, message: "superseded by import of -[NSObject init]")
  convenience init()
  convenience init(dummyParam: ())
  
  convenience init(cc x: Any)
  convenience init(_ x: Any)
  
  convenience init(aa a: Any, _ b: Any, cc c: Any)
  
  /*not inherited*/ init(fixedType: ())
  
  // Would-be initializers.
  class func zz() -> Self
  class func yy(aa x: Any) -> Self
  class func xx(_ x: Any, bb xx: Any) -> Self
  
  init()
}

class TestError : NSObject {
  
  convenience init(error: ()) throws
  convenience init(aa x: Any?, error: ()) throws
  convenience init(aa x: Any?, error: (), block: @escaping () -> Void) throws
  convenience init(error: (), block: @escaping () -> Void) throws
  
  convenience init(aa x: Any?) throws
  convenience init(aa x: Any?, block: @escaping () -> Void) throws
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
