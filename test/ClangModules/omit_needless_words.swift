// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify -Womit-needless-words -enable-infer-default-arguments %s

// REQUIRES: objc_interop

import Foundation
import AppKit

func renameFirst(vc: NSViewController) {
  vc.dismissAnimated(true) // expected-warning{{dismissAnimated' could be named 'dismiss(animated:)'}}{{6-21=dismiss}}{{22-22=animated: }}
}

func dropDefaultedNil(array: NSArray, sel: Selector,
       body: ((AnyObject!, Int, UnsafeMutablePointer<ObjCBool>) -> Void)?) {
  array.makeObjectsPerformSelector(sel, withObject: nil) // expected-warning{{'makeObjectsPerformSelector(_:withObject:)' could be named 'makeObjectsPerform(_:withObject:)'}}{{9-35=makeObjectsPerform}}
  array.makeObjectsPerformSelector(sel, withObject: nil, withObject: nil) // expected-warning{{'makeObjectsPerformSelector(_:withObject:withObject:)' could be named 'makeObjectsPerform(_:withObject:withObject:)'}}{{9-35=makeObjectsPerform}}
  array.enumerateObjectsRandomlyWithBlock(nil) // expected-warning{{'enumerateObjectsRandomlyWithBlock' could be named 'enumerateObjectsRandomly(withBlock:)'}}{{9-42=enumerateObjectsRandomly}}{{43-46=}}
  array.enumerateObjectsRandomlyWithBlock(body) // expected-warning{{'enumerateObjectsRandomlyWithBlock' could be named 'enumerateObjectsRandomly(withBlock:)'}}{{9-42=enumerateObjectsRandomly}}{{43-43=withBlock: }}
}

func dropDefaultedOptionSet(array: NSArray) {
  array.enumerateObjectsWithOptions([]) { obj, idx, stop in print("foo") } // expected-warning{{'enumerateObjectsWithOptions(_:usingBlock:)' could be named 'enumerateObjects(withOptions:usingBlock:)'}}{{9-36=enumerateObjects}}{{36-40=}}
  array.enumerateObjectsWithOptions([], usingBlock: { obj, idx, stop in print("foo") }) // expected-warning{{'enumerateObjectsWithOptions(_:usingBlock:)' could be named 'enumerateObjects(withOptions:usingBlock:)'}}{{9-36=enumerateObjects}}{{37-41=}}
  array.enumerateObjectsWhileOrderingPizza(true, withOptions: [], usingBlock: { obj, idx, stop in print("foo") }) // expected-warning{{'enumerateObjectsWhileOrderingPizza(_:withOptions:usingBlock:)' could be named 'enumerateObjectsWhileOrdering(pizza:withOptions:usingBlock:)'}}{{48-65=}}{{44-44=pizza: }}
}

func dropDefaultedWithoutRename(domain: String, code: Int, array: NSArray) {
  array.enumerateObjectsHaphazardly(nil) // expected-warning{{call to 'enumerateObjectsHaphazardly' has extraneous arguments that could use defaults}}{{37-40=}}
  array.optionallyEnumerateObjects([], body: { obj, idx, stop in print("foo") }) // expected-warning{{call to 'optionallyEnumerateObjects(_:body:)' has extraneous arguments that could use defaults}}{{36-40=}}
}

func dontDropUnnamedSetterArg(str: NSString) {
  str.setTextColor(nil) // don't drop this
}

func renameTrailingClosure(array: NSArray) {
  array.enumerateObjectsWithNullableBlock { _, _, _ in print("foo") } // expected-warning{{enumerateObjectsWithNullableBlock' could be named 'enumerateObjects(withNullableBlock:)' [-Womit-needless-words]}}{{9-42=enumerateObjects}}
}

