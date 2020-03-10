// The USR check definitions are in cross_language.swift.

#include "objc_header.h"

@interface MyCls1(ext_in_objc)
// CHECK: [[@LINE-1]]:12 | class/Swift | MyCls1 | [[MyCls1_USR]] |
// CHECK: [[@LINE-2]]:19 | extension/ObjC | ext_in_objc | c:@M@cross_language@objc(cy)MyCls1@ext_in_objc |
-(void)someMethFromObjC;
// CHECK: [[@LINE-1]]:8 | instance-method/ObjC | someMethFromObjC | [[someMethFromObjC_USR:.*]] | -[ext_in_objc someMethFromObjC]
@end

void test1() {
  MyCls1 *o = [[MyCls1 alloc] init];
  // CHECK: [[@LINE-1]]:3 | class/Swift | MyCls1 | [[MyCls1_USR]] |
  // CHECK: [[@LINE-2]]:31 | instance-method/Swift | init | [[MyCls1_init_USR]] |
  // CHECK: [[@LINE-3]]:17 | class/Swift | MyCls1 | [[MyCls1_USR]] |
  [o someMeth];
  // CHECK: [[@LINE-1]]:6 | instance-method/Swift | someMeth | [[MyCls1_someMeth_USR]] |
  [o someExtMeth];
  // CHECK: [[@LINE-1]]:6 | instance-method/Swift | someExtMeth | [[MyCls1_someExtMeth_USR]] |
  [o someMethFromObjC];
  // CHECK: [[@LINE-1]]:6 | instance-method/ObjC | someMethFromObjC | [[someMethFromObjC_USR]] |

  MyCls2 *o2 = [[MyCls2 alloc] initWithInt:0];
  // CHECK: [[@LINE-1]]:32 | instance-method/Swift | initWithInt: | [[MyCls2_initwithInt_USR]] |

  SomeObjCClass *oo;
  // CHECK: [[@LINE-1]]:3 | class/ObjC | SomeObjCClass | [[SomeObjCClass_USR]] |
  [oo someSwiftExtMeth];
  // CHECK: [[@LINE-1]]:7 | instance-method/Swift | someSwiftExtMeth | [[SomeObjCClass_someSwiftExtMeth_USR]] |

  id<MyProt> p;
  // CHECK: [[@LINE-1]]:6 | protocol/Swift | MyProt | [[MyProt_USR]] |
  [p someProtMeth];
  // CHECK: [[@LINE-1]]:6 | instance-method(protocol)/Swift | someProtMeth | [[MyProt_someProtMeth_USR]] |

  MyEnum myenm = MyEnumSomeEnumConst;
  // CHECK: [[@LINE-1]]:3 | enum/Swift | MyEnum | [[MyEnum_USR]] |
  // CHECK: [[@LINE-2]]:18 | enumerator/Swift | MyEnumSomeEnumConst | [[MyEnum_someEnumConst_USR]] |
}
