// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-clang %t/LibObjc.m -c -o %t/LibObjc.o %sdk

// RUN: %target-build-swift -emit-executable %t/Use.swift -g -import-objc-header %t/LibObjc.h -package-name pkg -Xlinker %t/LibObjc.o -o %t/a.out -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access -enable-library-evolution -O -wmo -emit-module -module-name Main -emit-module-path %t/Main.swiftmodule
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// RUN: %target-sil-opt %t/Main.swiftmodule -o %t/Main.sil
// RUN: %FileCheck %s < %t/Main.sil

// REQUIRES: executable_test
// REQUIRES: objc_interop


//--- LibObjc.h
#import <Foundation/Foundation.h>

@interface PropertyObjc: NSObject
- (instancetype)init;
@end

@interface KlassObjc: NSObject
@property (nonatomic, strong) PropertyObjc *propertyObjc;
- (instancetype)initWithArg:(PropertyObjc *)arg;
@end

@interface OtherObjc: NSObject
- (instancetype)init;
@end


//--- LibObjc.m
#include "LibObjc.h"

@implementation PropertyObjc
- (instancetype)init {
  return [super init];
}
@end

@implementation KlassObjc
@synthesize propertyObjc = _propertyObjc;

- (instancetype)initWithArg:(PropertyObjc *)arg {
  if (self = [super init]) {
    _propertyObjc = arg;
  }
  return self;
}
@end


//--- Use.swift

package struct PkgStruct {
  /// Pure ObjectiveC types are not serialized in Package CMO.
  // CHECK-NOT: s4Main9PkgStructV7objcVarSo9KlassObjcCvg
  // CHECK-NOT: s4Main9PkgStructV7objcVarSo9KlassObjcCvs
  // CHECK-NOT: s4Main9PkgStructV7objcVarSo9KlassObjcCvM
  /* --- NOT SERIALIZED ---
   sil package @$s4Main9PkgStructV7objcVarSo9KlassObjcCvg : $@convention(method) (@in_guaranteed PkgStruct) -> @owned KlassObjc {
   [%0: escape v** -> %r.v**, escape v**.c*.v** -> %r.v**.c*.v**, read s0.v**, copy v**]
   [global: copy]
   // %0 "self"                                      // user: %1
   bb0(%0 : $*PkgStruct):
     %1 = struct_element_addr %0 : $*PkgStruct, #PkgStruct.objcVar // user: %2
     %2 = load %1 : $*KlassObjc                      // users: %4, %3
     strong_retain %2 : $KlassObjc                   // id: %3
     return %2 : $KlassObjc                          // id: %4
   } // end sil function '$s4Main9PkgStructV7objcVarSo9KlassObjcCvg'
   */
  package var objcVar: KlassObjc

  // CHECK-NOT: s4Main9PkgStructV11castObjcVaryXlvg
  /* --- NOT SERIALIZED ---
   sil package @$s4Main9PkgStructV11castObjcVaryXlvg : $@convention(method) (@in_guaranteed PkgStruct) -> @owned AnyObject {
   [%0: read s0.v**, copy v**]
   [global: read,write,copy,destroy,allocate,deinit_barrier]
   // %0 "self"                                      // user: %1
   bb0(%0 : $*PkgStruct):
     %1 = struct_element_addr %0 : $*PkgStruct, #PkgStruct.objcVar // user: %2
     %2 = load %1 : $*KlassObjc                      // users: %3, %4
     %3 = objc_method %2 : $KlassObjc, #KlassObjc.propertyObjc!getter.foreign : (KlassObjc) -> () -> PropertyObjc?, $@convention(objc_method) (KlassObjc) -> @autoreleased Optional<PropertyObjc> // user: %4
     %4 = apply %3(%2) : $@convention(objc_method) (KlassObjc) -> @autoreleased Optional<PropertyObjc> // users: %9, %7
     // function_ref Optional._bridgeToObjectiveC()
     %5 = function_ref @$sSq19_bridgeToObjectiveCyXlyF : $@convention(method) <τ_0_0> (@in_guaranteed Optional<τ_0_0>) -> @owned AnyObject // user: %8
     %6 = alloc_stack $Optional<PropertyObjc>        // users: %8, %7, %10
     store %4 to %6 : $*Optional<PropertyObjc>       // id: %7
     %8 = apply %5<PropertyObjc>(%6) : $@convention(method) <τ_0_0> (@in_guaranteed Optional<τ_0_0>) -> @owned AnyObject // user: %11
     release_value %4 : $Optional<PropertyObjc>      // id: %9
     dealloc_stack %6 : $*Optional<PropertyObjc>     // id: %10
     return %8 : $AnyObject                          // id: %11
   } // end sil function '$s4Main9PkgStructV11castObjcVaryXlvg'
   */
  package var castObjcVar: AnyObject { objcVar.propertyObjc as AnyObject }
  // CHECK-NOT: s4Main9PkgStructV15castObjcVarBackSo08PropertyE0CSgvg
  package var castObjcVarBack: PropertyObjc? { (objcVar.propertyObjc as AnyObject) as? PropertyObjc }

  // CHECK-NOT: s4Main9PkgStructVyACSo9KlassObjcC_AA0D5SwiftCtcfC
  package init(_ objcArg: KlassObjc, _ swiftArg: KlassSwift) {
    objcVar = objcArg
    swiftVar = swiftArg
  }

  /// Swift types with @objc can be serialized.
  // CHECK-DAG: sil package [serialized_for_package] [canonical] @$s4Main9PkgStructV8swiftVarAA10KlassSwiftCvg : $@convention(method) (@in_guaranteed PkgStruct) -> @owned KlassSwift {
  package var swiftVar: KlassSwift
  // CHECK-DAG: sil package [serialized_for_package] [canonical] @$s4Main9PkgStructV23castVarWithObjcAttrBackAA013PropertySwiftgH0CSgvg : $@convention(method) (@in_guaranteed PkgStruct) -> @owned Optional<PropertySwiftObjcAttr> {
  package var castVarWithObjcAttr: AnyObject { swiftVar.propertyWithObjcAttr as AnyObject }
  // CHECK-DAG: sil package [serialized_for_package] [canonical] @$s4Main9PkgStructV23castVarWithObjcAttrBackAA013PropertySwiftgH0CSgvg : $@convention(method) (@in_guaranteed PkgStruct) -> @owned Optional<PropertySwiftObjcAttr> {
  package var castVarWithObjcAttrBack: PropertySwiftObjcAttr? { (swiftVar.propertyWithObjcAttr as AnyObject) as? PropertySwiftObjcAttr }
}


@objc
public class PropertySwiftObjcAttr: NSObject {
  // CHECK-DAG: sil [serialized_for_package] [canonical] @$s4Main21PropertySwiftObjcAttrC6intVarSivg : $@convention(method) (@guaranteed PropertySwiftObjcAttr) -> Int {
  public var intVar: Int = 1
}

public class KlassSwift {
  // CHECK-DAG: sil [serialized_for_package] [canonical] @$s4Main10KlassSwiftC20propertyWithObjcAttrAA08PropertycfG0Cvg : $@convention(method) (@guaranteed KlassSwift) -> @owned PropertySwiftObjcAttr {
  public var propertyWithObjcAttr: PropertySwiftObjcAttr
  public init(arg: PropertySwiftObjcAttr) {
    propertyWithObjcAttr = arg
  }
}

// CHECK: sil package_external [canonical] @$s4Main7pkgFuncyyF : $@convention(thin) () -> ()
package func pkgFunc() {
  /* --- NOT SERIALIZED ---
   sil package @$s4Main7pkgFuncyyF : $@convention(thin) () -> () {
   [global: read,write,copy,destroy,allocate,deinit_barrier]
   bb0:
     %0 = alloc_ref [objc] $PropertyObjc             // users: %1, %2
     %1 = objc_method %0 : $PropertyObjc, #PropertyObjc.init!initializer.foreign : (PropertyObjc.Type) -> () -> PropertyObjc?, $@convention(objc_method) (@owned PropertyObjc) -> @owned Optional<PropertyObjc> // user: %2
     %2 = apply %1(%0) : $@convention(objc_method) (@owned PropertyObjc) -> @owned Optional<PropertyObjc> // users: %6, %5
     %3 = alloc_ref [objc] $KlassObjc                // users: %4, %5
     %4 = objc_method %3 : $KlassObjc, #KlassObjc.init!initializer.foreign : (KlassObjc.Type) -> (PropertyObjc?) -> KlassObjc?, $@convention(objc_method) (Optional<PropertyObjc>, @owned KlassObjc) -> @owned Optional<KlassObjc> // user: %5
     %5 = apply %4(%2, %3) : $@convention(objc_method) (Optional<PropertyObjc>, @owned KlassObjc) -> @owned Optional<KlassObjc> // user: %7
     release_value %2 : $Optional<PropertyObjc>      // id: %6
     switch_enum %5 : $Optional<KlassObjc>, case #Optional.some!enumelt: bb2, case #Optional.none!enumelt: bb1 // id: %7
   ...
   bb2(%11 : $KlassObjc):                            // Preds: bb0
     %12 = alloc_ref [objc] $PropertySwiftObjcAttr   // users: %13, %14
     %13 = objc_method %12 : $PropertySwiftObjcAttr, #PropertySwiftObjcAttr.init!initializer.foreign : (PropertySwiftObjcAttr.Type) -> () -> PropertySwiftObjcAttr, $@convention(objc_method) (@owned PropertySwiftObjcAttr) -> @owned PropertySwiftObjcAttr // user: %14
     %14 = apply %13(%12) : $@convention(objc_method) (@owned PropertySwiftObjcAttr) -> @owned PropertySwiftObjcAttr // users: %20, %16
     %15 = alloc_ref [stack] $KlassSwift             // users: %116, %141, %101, %18, %17
     debug_value %14 : $PropertySwiftObjcAttr, let, name "arg", argno 1 // id: %16
     debug_value %15 : $KlassSwift, let, name "self", argno 2 // id: %17
     %18 = end_init_let_ref %15 : $KlassSwift        // users: %57, %106, %28, %27, %23, %19
     %19 = ref_element_addr %18 : $KlassSwift, #KlassSwift.propertyWithObjcAttr // users: %107, %58, %20
     store %14 to %19 : $*PropertySwiftObjcAttr      // id: %20
     %21 = alloc_stack [lexical] [var_decl] $PkgStruct, var, name "self" // users: %29, %26, %24
     debug_value %11 : $KlassObjc, let, name "objcArg", argno 1 // id: %22
     debug_value %18 : $KlassSwift, let, name "swiftArg", argno 2 // id: %23
     %24 = struct_element_addr %21 : $*PkgStruct, #PkgStruct.objcVar // user: %25
     store %11 to %24 : $*KlassObjc                  // id: %25
     %26 = struct_element_addr %21 : $*PkgStruct, #PkgStruct.swiftVar // user: %27
     store %18 to %26 : $*KlassSwift                 // id: %27
     %28 = struct $PkgStruct (%11 : $KlassObjc, %18 : $KlassSwift) // users: %50, %104, %99, %120, %114, %140, %110, %90, %43, %100, %115, %30
      dealloc_stack %21 : $*PkgStruct                 // id: %29
     ...
   */
  let p = PkgStruct(KlassObjc(arg: PropertyObjc()), KlassSwift(arg: PropertySwiftObjcAttr()))
  print(p.castObjcVar, p.castVarWithObjcAttr)
  if let a = p.castObjcVarBack,
     let b = p.castVarWithObjcAttrBack {
    print(a, b)
  }
}

// Should not crash.
pkgFunc()
