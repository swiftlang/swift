// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -I=%S/Inputs/custom-modules -parse -parse-as-library -verify -module-cache-path=%t/clang-module-cache -triple x86_64-apple-darwin13 %s

import CoreData

class MyManagedObject : NSManagedObject {
  var foo: String // expected-error{{stored property 'foo' requires an initial value}}
  var bar: String // expected-error{{stored property 'bar' requires an initial value}}
}
