// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-clang -fobjc-arc %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -Xfrontend -enable-import-objc-generics -I %S/Inputs/ObjCClasses/ -Xlinker %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import ObjCClasses

let cs = Container<NSString>(object: "i-just-met-you")
print(cs)
// CHECK: <Container: {{.*}}>
print(cs.object)
// CHECK: i-just-met-you

cs.processObjectWithBlock { s in print("in block: \(s)!!!") }
// CHECK: in block: i-just-met-you!!!
cs.getObjectWithBlock { () in NSString(string: "this-is-crazy") }
print(cs.object)
// CHECK: this-is-crazy
cs.object = "so-heres-my-number"
print(cs.object)
// CHECK: so-heres-my-number

let cs2 = Container<NSString>(object: "call-me-maybe")
print(cs2.getCat1())
// CHECK: call-me-maybe
cs2.setCat1(":)")
print(cs2.cat1Property)
// CHECK: :)

let scs = SubContainer<NSString>(object: "hey neat")
print(scs.object)
// CHECK: hey neat 

let nestedContainer = NestedContainer<NSString>(object: Container<NSString>(object: "cat"))
print(nestedContainer.object.object)
// CHECK: cat

let stringContainer = StringContainer(object: "stringy")
print(stringContainer.object)
// CHECK: stringy

// TODO: resolve crash in IRGen from test case below
/*func makeContainer<T: AnyObject>(x: T) -> Container<T> {*/
  /*return Container(object: x)*/
/*}*/

func openContainer<T: AnyObject>(x: Container<T>) -> T {
  return x.object
}
print(openContainer(Container<NSString>(object: "some string")))
// CHECK: some string

func openStringContainer<T: Container<NSString>>(x: T) -> NSString {
  return x.object
}
print(openStringContainer(Container<NSString>(object: "some string")))
// CHECK: some string
print(openStringContainer(SubContainer<NSString>(object: "some string 2")))
// CHECK: some string 2
print(openStringContainer(StringContainer(object: "some string 3")))
// CHECK: some string 3

func openArbitraryContainer<S: AnyObject, T: Container<S>>(x: T) -> S {
  return x.object
}
print(openArbitraryContainer(Container<NSString>(object: "some string 4")))
// CHECK: some string 4
print(openArbitraryContainer(Container<NSNumber>(object: NSNumber(integer: 5))) as NSNumber)
// CHECK: 5

let _ = CopyingContainer<NSString>()
func copyContainerContents<T: NSCopying>(x: CopyingContainer<T>) -> T {
  return x.object.copyWithZone(nil) as! T
}

func makeContainedAnimalMakeNoise<T: Animal>(x: AnimalContainer<T>) {
  x.object.makeNoise()
}
makeContainedAnimalMakeNoise(AnimalContainer(object: Dog()))
// CHECK: woof
