// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify -Womit-needless-words -enable-infer-default-arguments %s

// REQUIRES: objc_interop

import Foundation
import AppKit

func dropDefaultedNil(array: NSArray, sel: Selector,
       body: ((AnyObject!, Int, UnsafeMutablePointer<ObjCBool>) -> Void)?) {
  array.makeObjectsPerformSelector(sel, withObject: nil) // expected-warning{{'makeObjectsPerformSelector(_:withObject:)' could be named 'makeObjectsPerform(_:with:)'}}{{9-35=makeObjectsPerform}}
  array.makeObjectsPerformSelector(sel, withObject: nil, withObject: nil) // expected-warning{{'makeObjectsPerformSelector(_:withObject:withObject:)' could be named 'makeObjectsPerform(_:with:with:)'}}{{9-35=makeObjectsPerform}}
  array.enumerateObjectsRandomlyWithBlock(nil) // expected-warning{{'enumerateObjectsRandomlyWithBlock' could be named 'enumerateObjectsRandomly(block:)'}}{{9-42=enumerateObjectsRandomly}}{{43-46=}}
  array.enumerateObjectsRandomlyWithBlock(body) // expected-warning{{'enumerateObjectsRandomlyWithBlock' could be named 'enumerateObjectsRandomly(block:)'}}{{9-42=enumerateObjectsRandomly}}{{43-43=block: }}
}

func dropDefaultedOptionSet(array: NSArray) {
  array.enumerateObjectsWithOptions([]) { obj, idx, stop in print("foo") } // expected-warning{{'enumerateObjectsWithOptions(_:usingBlock:)' could be named 'enumerateObjects(_:usingBlock:)'}}{{9-36=enumerateObjects}}{{36-40=}}
  array.enumerateObjectsWithOptions([], usingBlock: { obj, idx, stop in print("foo") }) // expected-warning{{'enumerateObjectsWithOptions(_:usingBlock:)' could be named 'enumerateObjects(_:usingBlock:)'}}{{9-36=enumerateObjects}}{{37-41=}}
  array.enumerateObjectsWhileOrderingPizza(true, withOptions: [], usingBlock: { obj, idx, stop in print("foo") }) // expected-warning{{enumerateObjectsWhileOrderingPizza(_:withOptions:usingBlock:)' could be named 'enumerateObjectsWhileOrderingPizza(_:with:usingBlock:)'}}{{48-65=}}
}

func dropDefaultedWithoutRename(domain: String, code: Int, array: NSArray) {
  array.enumerateObjectsHaphazardly(nil) // expected-warning{{call to 'enumerateObjectsHaphazardly' has extraneous arguments that could use defaults}}{{37-40=}}
  array.optionallyEnumerateObjects([], body: { obj, idx, stop in print("foo") }) // expected-warning{{call to 'optionallyEnumerateObjects(_:body:)' has extraneous arguments that could use defaults}}{{36-40=}}
}

func dontDropUnnamedSetterArg(str: NSString) {
  str.setTextColor(nil) // don't drop this
}

func renameTrailingClosure(array: NSArray) {
  array.enumerateObjectsWithNullableBlock { _, _, _ in print("foo") } // expected-warning{{enumerateObjectsWithNullableBlock' could be named 'enumerateObjects(nullableBlock:)' [-Womit-needless-words]}}{{9-42=enumerateObjects}}
}

