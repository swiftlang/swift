protocol [class_protocol, objc] ObjCProto {
  func doSomething()
}

class [objc] ObjCClass {
  static func classMethod() {}
  func implicitlyObjC() {}

  var [iboutlet] outlet : ObjCClass
  func [ibaction] performAction() {}
}

class NonObjCClass : ObjCProto {
  func doSomething() {}
  
  func [objc] objcMethod() {}
  var [objc] objcProp : ObjCClass
  
  subscript [objc] (i : Int) -> () {
    return ()
  }

  var [iboutlet] outlet : ObjCClass
  func [ibaction] performAction() {}
}
