// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -parse -parse-as-library -verify %s %S/Inputs/objc_nsmanaged_other.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -parse -parse-as-library -verify -primary-file %s %S/Inputs/objc_nsmanaged_other.swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -emit-silgen -parse-as-library -o /dev/null -DNO_ERROR %s %S/Inputs/objc_nsmanaged_other.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -emit-silgen -parse-as-library -o /dev/null -DNO_ERROR -primary-file %s %S/Inputs/objc_nsmanaged_other.swift

// REQUIRES: objc_interop

import Foundation
import CoreData

func markUsed<T>(t: T) {}

// Inferred @requires_stored_property_inits.
class MyManagedObject : NSManagedObject {
#if !NO_ERROR
  var foo: String // expected-error{{stored property 'foo' requires an initial value}}
  var bar: String // expected-error{{stored property 'bar' requires an initial value}}
#endif

  @NSManaged var managed: String 

  override init() { 
#if !NO_ERROR
    foo = "1"
    bar = "2"
#endif
    super.init() 
  }

  var wibble: String {
    return "hello"
  }

#if !NO_ERROR
  var wobble: String { // expected-error{{stored property 'wobble' requires an initial value}}
    willSet(value) {
      markUsed(value)
    }
  }
#endif
}

func getOtherManagedObject() -> OtherManagedObject {
  return OtherManagedObject()
}

func accessOther(om: OtherManagedObject) -> String {
  return om.managed
}

// rdar://problem/20821582
func accessMine(obj: MyManagedObject) -> String {
  return obj.anotherManaged
}
