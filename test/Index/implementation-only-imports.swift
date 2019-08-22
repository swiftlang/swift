// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend -emit-module -o %t -module-name CoreFoo %S/Inputs/implementation-only-imports-helper.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t -module-name FooKit %s
// RUN: %target-swift-ide-test -print-indexed-symbols -module-name FooKit -source-filename %s -I %t > %t.out

// RUN: mv %t/CoreFoo.swiftmodule %t/SecretCoreFoo.swiftmodule
// RUN: echo '---FROM MODULE---' >> %t.out
// RUN: %target-swift-ide-test -print-indexed-symbols -module-to-print FooKit -source-filename %s -I %t >> %t.out
// RUN: %FileCheck %s -input-file=%t.out
// RUN: %FileCheck -check-prefix NEGATIVE %s -input-file=%t.out

// CHECK: module | user | CoreFoo | {{.+}}CoreFoo.swiftmodule
@_implementationOnly import CoreFoo
// CHECK: [[@LINE-1]]:{{[0-9]+}} | module/Swift | CoreFoo | c:@M@CoreFoo | Ref | rel: 0

internal class SECRETSubclass : SECRETClass {}
// CHECK: [[@LINE-1]]:{{[0-9]+}} | class/Swift | SECRETSubclass | [[SECRET_SUBCLASS_USR:s:[^|]+]] | Def | rel: 0
// CHECK: [[@LINE-2]]:{{[0-9]+}} | class/Swift | SECRETClass | [[SECRET_CLASS_USR:s:[^|]+]] | Ref,RelBase | rel: 1
// CHECK-NEXT:  RelBase | class/Swift | SECRETSubclass | [[SECRET_SUBCLASS_USR]]{{$}}

public class PublicClass {
  // CHECK: [[@LINE-1]]:{{[0-9]+}} | class/Swift | PublicClass | [[PUBLIC_CLASS_USR:s:[^|]+]] | Def | rel: 0
  internal var propSECRET: SECRETClass? { nil }
  // CHECK: [[@LINE-1]]:{{[0-9]+}} | instance-property/Swift | propSECRET |
  internal var propSECRET2: SECRETSubclass? { nil }
  // CHECK: [[@LINE-1]]:{{[0-9]+}} | instance-property/Swift | propSECRET2 |
}

extension SECRETClass {
  // CHECK: [[@LINE-1]]:{{[0-9]+}} | extension/ext-class/Swift | SECRETClass |
  internal func testSECRET() {}
  // CHECK: [[@LINE-1]]:{{[0-9]+}} | instance-method/Swift | testSECRET() |
}

extension PublicClass {
  // CHECK: [[@LINE-1]]:{{[0-9]+}} | extension/ext-class/Swift | PublicClass | [[PUBLIC_CLASS_EXTENSION_USR:s:[^|]+]] |
  public func extensionNamingMethod() {}
  // CHECK: [[@LINE-1]]:{{[0-9]+}} | instance-method/Swift | extensionNamingMethod() |

  internal func testSECRET() {}
  // CHECK: [[@LINE-1]]:{{[0-9]+}} | instance-method/Swift | testSECRET() |

  internal struct SECRETStruct: SECRETProtocol {}
  // CHECK: [[@LINE-1]]:{{[0-9]+}} | struct/Swift | SECRETStruct | [[SECRET_NESTED_STRUCT_USR:s:[^|]+]] |
  // CHECK: [[@LINE-2]]:{{[0-9]+}} | protocol/Swift | SECRETProtocol | [[SECRET_PROTOCOL_USR:s:[^|]+]] | Ref,RelBase | rel: 1
  // CHECK-NEXT: RelBase | struct/Swift | SECRETStruct | [[SECRET_NESTED_STRUCT_USR]]
}

extension Optional where Wrapped: SECRETProtocol {
  // CHECK: [[@LINE-1]]:{{[0-9]+}} | extension/ext-enum/Swift | Optional |
  internal func extensionNamingMethod() {}
  // CHECK: [[@LINE-1]]:{{[0-9]+}} | instance-method/Swift | extensionNamingMethod() |

  internal func testSECRET() {}
  // CHECK: [[@LINE-1]]:{{[0-9]+}} | instance-method/Swift | testSECRET() |
}

// CHECK-LABEL: ---FROM MODULE---
// NEGATIVE-LABEL: ---FROM MODULE---

// CHECK: 0:0 | class/Swift | PublicClass | [[PUBLIC_CLASS_USR]] | Def | rel: 0
// CHECK: 0:0 | extension/ext-class/Swift | PublicClass | [[PUBLIC_CLASS_EXTENSION_USR]] | Def | rel: 0
// CHECK: 0:0 | class/Swift | PublicClass | [[PUBLIC_CLASS_USR]] | Ref,RelExt | rel: 1
// CHECK-NEXT: RelExt | extension/ext-class/Swift | PublicClass | [[PUBLIC_CLASS_EXTENSION_USR]]


// NEGATIVE-NOT: CoreFoo
// NEGATIVE-NOT: SECRET
