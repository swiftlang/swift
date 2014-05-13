// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -I=%S/Inputs/custom-modules -parse -parse-as-library -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 %s %S/Inputs/objc_nsmanaged_other.swift

import CoreData

// Inferred @requires_stored_property_inits.
class MyManagedObject : NSManagedObject {
  var foo: String // expected-error{{stored property 'foo' requires an initial value}}
  var bar: String // expected-error{{stored property 'bar' requires an initial value}}
  
  @NSManaged var managed: String 

  init() { 
    foo = "1"
    bar = "2"
    super.init() 
  }

  var wibble: String {
    return "hello"
  }

  var wobble: String { // expected-error{{stored property 'wobble' requires an initial value}}
    willSet(value) {
      println(value)
    }
  }
}

func accessOther(om: OtherManagedObject) -> String {
  return om.managed
}
