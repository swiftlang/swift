import Foundation

@objc protocol Proto {
  func implicitObjCMethod()
}

class Object : Proto {
  func implicitObjCMethod() {}

  @objc dynamic
  func objCMethod() {}

  @objc dynamic
  var objcProperty : Int {
    return 0
  }
  @objc dynamic
  var objcProperty2 : Int = 0
}
