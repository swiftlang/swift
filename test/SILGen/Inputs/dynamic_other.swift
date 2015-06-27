import Foundation

class FromOtherFile: Proto {
  // Not objc or dynamic, so only a vtable entry
  init(native: Int) {}
  func nativeMethod() {}
  var nativeProp: Int = 0
  subscript(native native: Int) -> Int {
    get { return native }
    set {}
  }

  // @objc, so it has an ObjC entry point but can also be dispatched
  // by vtable
  @objc init(objc: Int) {}
  @objc func objcMethod() {}
  @objc var objcProp: Int = 0
  @objc subscript(objc objc: Int) -> Int {
    get { return objc }
    set {}
  }

  // dynamic, so it has only an ObjC entry point
  dynamic init(dynamic: Int) {}
  dynamic func dynamicMethod() {}
  dynamic var dynamicProp: Int = 0
  dynamic subscript(dynamic dynamic: Int) -> Int {
    get { return dynamic }
    set {}
  }

  func overriddenByDynamic() {}

  @NSManaged var managedProp: Int
}

@objc class ObjCOtherFile : NSObject {}

extension ObjCOtherFile {
  func extensionMethod() {}
  var extensionProp: Int { return 0 }
  class var extensionClassProp: Int { return 0 }

  dynamic func dynExtensionMethod() {}
  dynamic var dynExtensionProp: Int { return 0 }
  dynamic class var dynExtensionClassProp: Int { return 0 }
}
