// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/swift)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/swift/SwiftLibrary.swiftmodule %s -module-name SwiftLibrary -I%S/Inputs
// RUN: %target-swift-frontend -verify -DCLIENT -c %s -module-name client -I%t/swift -o /dev/null

#if CLIENT
import SwiftLibrary

public class MyObject: Object {
}
#else
@_implementationOnly import CLibrary

open class Object {
  internal var storage: AnyObject
  internal var raw: CObject { unsafeBitCast(storage, to: CObject.self) }

  fileprivate init(object: CObject) {
    self.storage = object
  }
}
#endif
