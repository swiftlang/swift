// RUN: %target-swift-frontend -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -emit-silgen | FileCheck %s

import Foundation

@objc protocol ObjCReadOnly {
  var name : String { get }
}

@objc protocol ObjCReadWrite {
  var name : String { get set }
}

class SomeObject : NSObject, ObjCReadOnly, ObjCReadWrite {
  @NSManaged var name : String
}

// We should not emit references to native Swift accessors for @NSManaged
// properties.
// CHECK-NOT: hidden_external {{.*}}main{{.*}}SomeObject{{.*}}name
