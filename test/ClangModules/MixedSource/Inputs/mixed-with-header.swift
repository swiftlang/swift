@objc class ForwardClass : NSObject {
}

@objc protocol ForwardProto : NSObjectProtocol {
}
@objc class ForwardProtoAdopter : NSObject, ForwardProto {
}

@objc class PartialBaseClass {
}
@objc class PartialSubClass : NSObject {
}

class ProtoConformer : ForwardClassUser {
   func consumeForwardClass(arg: ForwardClass) {}

   var forward = ForwardClass()
}

func testProtocolWrapper(conformer: ForwardClassUser) {
   conformer.consumeForwardClass(conformer.forward)
}

func testStruct(p: Point) -> Point {
   var result = p
   result.y += 5
   return result
}

class Derived : Base {
   override func safeOverride(arg: NSObject) -> ForwardClass {
      return ForwardClass()
   }
}

func rdar16923405(a: AALevel) {}

