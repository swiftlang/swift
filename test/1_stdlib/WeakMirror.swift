//===--- Mirror.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: if [ %target-runtime == "objc" ]; \
// RUN: then \
// RUN:   %target-clang %S/Inputs/Mirror/Mirror.mm -c -o %t/Mirror.mm.o -g && \
// RUN:   %target-build-swift -Xfrontend -disable-access-control %s -I %S/Inputs/Mirror/ -Xlinker %t/Mirror.mm.o -o %t/Mirror; \
// RUN: else \
// RUN:   %target-build-swift %s -Xfrontend -disable-access-control -o %t/Mirror; \
// RUN: fi
// RUN: %target-run %t/Mirror
// REQUIRES: executable_test

import StdlibUnittest

var mirrors = TestSuite("Mirrors")

class NativeSwiftClass : NativeClassBoundExistential {}

protocol NativeClassBoundExistential : class {}
class NativeSwiftClassHasWeak {
  weak var weakProperty: AnyObject?
}

class NativeSwiftClassHasNativeClassBoundExistential {
  weak var weakProperty: NativeClassBoundExistential?
}

struct StructHasNativeWeakReference {
  weak var weakProperty: AnyObject?
}

mirrors.test("class/NativeSwiftClassHasNativeWeakReference") {
  let c1 = NativeSwiftClassHasWeak()
  let c2 = NativeSwiftClass()
  c1.weakProperty = c2
  let classChild = _reflect(c1)[0].1.value
  print(classChild)
}

mirrors.test("class/NativeSwiftClassHasNativeClassBoundExistential") {
  let c1 = NativeSwiftClassHasNativeClassBoundExistential()
  let e = NativeSwiftClass() as NativeClassBoundExistential
  c1.weakProperty = e
  let classChild = _reflect(c1)[0].1.value
  print(classChild)
}

mirrors.test("struct/StructHasNativeWeakReference") {
  var s = StructHasNativeWeakReference()
  let c2 = NativeSwiftClass()
  s.weakProperty = c2
  let structChild = _reflect(s)[0].1.value
  print(structChild)
}

#if _runtime(_ObjC)

import Foundation

@objc protocol ObjCClassBoundExistential : class {}

@objc protocol ObjCProtocol : class {
  weak var weakProperty: AnyObject? { get set }
}

class NativeSwiftClassHasObjCClassBoundExistential {
  weak var weakProperty: NSObjectProtocol?
}

class ObjCClassHasWeak : NSObject {
  weak var weakProperty: AnyObject?
}

class ObjCClassHasNativeClassBoundExistential : NSObject {
  weak var weakProperty: NativeClassBoundExistential?
}

class ObjCClassHasObjCClassBoundExistential : NSObject {
  weak var weakProperty: NSObjectProtocol?
}

struct StructHasObjCWeakReference {
  weak var weakProperty: NSObject?
}

struct StructHasObjCClassBoundExistential {
  weak var weakProperty: NSObjectProtocol?
}

mirrors.test("class/NativeSwiftClassHasObjCWeakReference") {
  let c1 = NativeSwiftClassHasWeak()
  let nso = NSObject()
  c1.weakProperty = nso
  let classChild = _reflect(c1)[0].1.value
  print(classChild)
}

mirrors.test("class/NativeSwiftClassHasObjCClassBoundExistential") {
  let c1 = NativeSwiftClassHasObjCClassBoundExistential()
  let nso = NSObject() as NSObjectProtocol
  c1.weakProperty = nso
  let classChild = _reflect(c1)[0].1.value
  print(classChild)
}

mirrors.test("class/ObjCClassHasNativeWeak") {
  let c1 = ObjCClassHasWeak()
  let c2 = NativeSwiftClass()
  c1.weakProperty = c2
  let classChild = _reflect(c1)[0].1.value
  print(classChild)
}

mirrors.test("class/ObjcCClassHasObjCWeakReference") {
  let c1 = ObjCClassHasWeak()
  let nso = NSObject()
  c1.weakProperty = nso
  let classChild = _reflect(c1)[0].1.value
  print(classChild)
}

mirrors.test("class/ObjCClassHasNativeClassBoundExistential") {
  let c1 = ObjCClassHasNativeClassBoundExistential()
  let e = NativeSwiftClass() as NativeClassBoundExistential
  c1.weakProperty = e
  let classChild = _reflect(c1)[0].1.value
  print(classChild)
}

mirrors.test("class/ObjCClassHasObjCClassBoundExistential") {
  let c1 = ObjCClassHasObjCClassBoundExistential()
  let nsop = NSObject() as NSObjectProtocol
  c1.weakProperty = nsop
  let classChild = _reflect(c1)[0].1.value
  print(classChild)
}

mirrors.test("struct/StructHasObjCWeakReference") {
  var s = StructHasObjCWeakReference()
  let nso = NSObject()
  s.weakProperty = nso
  let structChild = _reflect(s)[0].1.value
  print(structChild)
}

mirrors.test("struct/StructHasObjCClassBoundExistential") {
  var s = StructHasObjCClassBoundExistential()
  let nsop = NSObject() as NSObjectProtocol
  s.weakProperty = nsop
  let structChild = _reflect(s)[0].1.value
  print(structChild)
}

#endif

runAllTests()
