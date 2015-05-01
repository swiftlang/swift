// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -parse -parse-as-library -verify %s %S/Inputs/objc_nsmanaged_other.swift

// REQUIRES: objc_interop

import CoreData

func markUsed<T>(t: T) {}

// Inferred @requires_stored_property_inits.
class MyManagedObject : NSManagedObject {
  var foo: String // expected-error{{stored property 'foo' requires an initial value}}
  var bar: String // expected-error{{stored property 'bar' requires an initial value}}
  
  @NSManaged var managed: String 

  override init() { 
    foo = "1"
    bar = "2"
    super.init() 
  }

  var wibble: String {
    return "hello"
  }

  var wobble: String { // expected-error{{stored property 'wobble' requires an initial value}}
    willSet(value) {
      markUsed(value)
    }
  }
}

func getOtherManagedObject() -> OtherManagedObject {
  return OtherManagedObject()
}

func accessOther(om: OtherManagedObject) -> String {
  return om.managed
}
