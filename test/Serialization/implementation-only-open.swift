// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/swift)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/swift/SwiftLibrary.swiftmodule %s -module-name SwiftLibrary -I%S/Inputs
// RUN: %target-typecheck-verify-swift -DCLIENT -c %s -module-name client -I%t/swift

#if CLIENT
import SwiftLibrary

public class MyObject: Object {
// expected-error@-1 {{cannot inherit from class 'Object' because it has overridable members that could not be loaded}}
// expected-note@-2 {{could not deserialize 'raw'}}
// expected-note@-3 {{could not deserialize 'init(object:)'}}
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
