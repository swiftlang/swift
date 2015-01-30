// RUN: %target-swift-frontend -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -emit-silgen | FileCheck %s

import Foundation

@objc protocol ObjCReadOnly {
  var name : String { get }
}

class SomeObject : NSObject, ObjCReadOnly {
  @NSManaged var name : String
}

// We should not emit references to native Swift accessors for @NSManaged
// properties.
// CHECK-NOT: _TFC4main10SomeObject{{.}}4nameSS
