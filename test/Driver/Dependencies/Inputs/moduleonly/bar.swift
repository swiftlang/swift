public func bar() { }

public class Bar {
  @usableFromInline internal class Inner {

    /// makeX is declared in foo.swift
    var x = makeX([1,2,3,4])

    @usableFromInline internal func f() {
      print("Bar.Inner.f")
    }
  }
}

#if _runtime(_ObjC) && canImport(Foundation)

import Foundation

public class ObjCBar : NSObject {

  @objc(foo)
  public func bar() -> String {
    return ""
  }

}

#endif
