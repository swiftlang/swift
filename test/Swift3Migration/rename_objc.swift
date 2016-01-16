// RUN: %swift -parse -target %target-triple %s -fixit-all -emit-fixits-path %t.remap -swift3-migration -enable-source-import -sdk %S/../Inputs/clang-importer-sdk -I %S/../Inputs/clang-importer-sdk/swift-modules -enable-infer-default-arguments
// RUN: c-arcmt-test %t.remap | arcmt-test -verify-transformed-files %s.result

// REQUIRES: objc_interop

import Foundation
import AppKit

func dropDefaultedNil(array: NSArray, sel: Selector,
       body: ((AnyObject!, Int, UnsafeMutablePointer<ObjCBool>) -> Void)?) {
  array.makeObjectsPerformSelector(sel, withObject: nil)
  array.makeObjectsPerformSelector(sel, withObject: nil, withObject: nil)
  array.enumerateObjectsRandomlyWithBlock(nil)
  array.enumerateObjectsRandomlyWithBlock(body)
}

func dropDefaultedOptionSet(array: NSArray) {
  array.enumerateObjectsWithOptions([]) { obj, idx, stop in print("foo") }
  array.enumerateObjectsWithOptions([], usingBlock: { obj, idx, stop in print("foo") })
  array.enumerateObjectsWhileOrderingPizza(true, withOptions: [], usingBlock: { obj, idx, stop in print("foo") })
}

func dropDefaultedWithoutRename(domain: String, code: Int, array: NSArray) {
  array.enumerateObjectsHaphazardly(nil)
  array.optionallyEnumerateObjects([], body: { obj, idx, stop in print("foo") })
}

func dontDropUnnamedSetterArg(str: NSString) {
  str.setTextColor(nil) // don't drop this
}

func renameTrailingClosure(array: NSArray) {
  array.enumerateObjectsWithNullableBlock { _, _, _ in print("foo") }
}

func useAnyObject(obj: AnyObject, body: ((AnyObject!, Int, UnsafeMutablePointer<ObjCBool>) -> Void)?) {
  obj.enumerateObjectsWithOptions([], usingBlock: { obj, idx, stop in print("foo") })
  obj.enumerateObjectsRandomlyWithBlock(nil)
  obj.enumerateObjectsRandomlyWithBlock(body)
  obj.enumerateObjectsRandomlyWithBlock?(nil)
  obj.enumerateObjectsRandomlyWithBlock?(body)
  _ = obj.makingHoney
}
