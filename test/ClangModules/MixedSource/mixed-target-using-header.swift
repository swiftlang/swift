// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift %clang-importer-sdk -target x86_64-apple-macosx10.9 -module-cache-path %t -I %S/../Inputs/custom-modules -import-objc-header %S/Inputs/mixed-target/header.h -parse %s -verify

func test(foo : FooProto) {
  let _: CInt = foo.bar
  let _: CInt = ExternIntX.x
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
  func consumeForwardClass(arg: ForwardClass) {}

  var forward = ForwardClass()
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
  let _: CInt = CONSTANT
}
