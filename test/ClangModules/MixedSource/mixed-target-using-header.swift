// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/../Inputs/custom-modules -import-objc-header %S/Inputs/mixed-target/header.h -parse %s -disable-objc-attr-requires-foundation-module -verify

// REQUIRES: objc_interop

func test(_ foo : FooProto) {
  _ = foo.bar as CInt
  _ = ExternIntX.x as CInt
}


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

func testCFunction() {
  doSomething(ForwardClass())
  doSomethingProto(ForwardProtoAdopter())
  doSomethingPartialBase(PartialBaseClass())
  doSomethingPartialSub(PartialSubClass())
}

class ProtoConformer : ForwardClassUser {
  @objc func consumeForwardClass(_ arg: ForwardClass) {}

  @objc var forward = ForwardClass()
}

func testProtocolWrapper(_ conformer: ForwardClassUser) {
  conformer.consumeForwardClass(conformer.forward)
}
testProtocolWrapper(ProtoConformer())

func testStruct(_ p: Point) -> Point {
  var result = p
  result.y += 5
  return result
}

func testSuppressed() {
  let _: __int128_t? = nil // expected-error{{use of undeclared type '__int128_t'}}
}

func testMacro() {
  _ = CONSTANT as CInt
}

func testFoundationOverlay() {
  _ = NSUTF8StringEncoding // no ambiguity
  _ = NSUTF8StringEncoding as UInt // and we should get the overlay version
}

func testProtocolNamingConflict() {
  let a: ConflictingName1? = nil
  var b: ConflictingName1Protocol?
  b = a // expected-error {{cannot assign value of type 'ConflictingName1?' to type 'ConflictingName1Protocol?'}}
  _ = b

  let c: ConflictingName2? = nil
  var d: ConflictingName2Protocol?
  d = c // expected-error {{cannot assign value of type 'ConflictingName2?' to type 'ConflictingName2Protocol?'}}
  _ = d
}

func testDeclsNestedInObjCContainers() {
  let _: NameInInterface = 0
  let _: NameInProtocol = 0
  let _: NameInCategory = 0
}
