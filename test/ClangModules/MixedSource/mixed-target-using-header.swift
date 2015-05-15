// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/../Inputs/custom-modules -import-objc-header %S/Inputs/mixed-target/header.h -parse %s -verify

// REQUIRES: objc_interop

func test(foo : FooProto) {
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
  @objc func consumeForwardClass(arg: ForwardClass) {}

  @objc var forward = ForwardClass()
}

func testProtocolWrapper(conformer: ForwardClassUser) {
  conformer.consumeForwardClass(conformer.forward)
}
testProtocolWrapper(ProtoConformer())

func testStruct(p: Point) -> Point {
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
