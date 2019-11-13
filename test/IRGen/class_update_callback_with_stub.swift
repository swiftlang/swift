// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -I %t %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-module-path %t/resilient_class.swiftmodule -enable-library-evolution %S/../Inputs/resilient_class.swift
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-module-path %t/resilient_objc_class.swiftmodule -enable-library-evolution %S/../Inputs/resilient_objc_class.swift
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -I %t -emit-ir -enable-library-evolution -target %target-next-stable-abi-triple %s > %t/out
// RUN: %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK-%target-runtime -DINT=i%target-ptrsize < %t/out
// RUN: %FileCheck %s --check-prefix=NEGATIVE < %t/out

import Foundation
import resilient_class
import resilient_objc_class

// REQUIRES: objc_interop
// REQUIRES: swift_stable_abi

// -- Nominal type descriptor for ResilientSubclass
// the interesting part is the 'extra class flags' field has a value of 1.

// CHECK-LABEL: @"$s31class_update_callback_with_stub17ResilientSubclassCMn" = constant <{
// -- flags
// CHECK-SAME: i32 1644232784, 
// -- parent
// CHECK-SAME: @"$s31class_update_callback_with_stubMXM"
// -- name
// CHECK-SAME: @1
// -- access function
// CHECK-SAME: @"$s31class_update_callback_with_stub17ResilientSubclassCMa"
// -- field descriptor
// CHECK-SAME: @"$s31class_update_callback_with_stub17ResilientSubclassCMF"
// -- superclass
// CHECK-SAME: @"symbolic{{[^"]*}}15resilient_class22ResilientOutsideParentC"
// -- metadata bounds
// CHECK-SAME: @"$s31class_update_callback_with_stub17ResilientSubclassCMo"
// -- extra class flags -- has Objective-C resilient class stub
// CHECK-SAME: i32 1,
// -- number of immediate members
// CHECK-SAME: i32 0,
// -- number of fields
// CHECK-SAME: i32 0,
// -- field offset vector offset
// CHECK-SAME: i32 0,
// -- resilient superclass
// CHECK-SAME: @"got.$s15resilient_class22ResilientOutsideParentCMn"
// -- singleton metadata initialization
// CHECK-SAME: @"$s31class_update_callback_with_stub17ResilientSubclassCMl"
// -- resilient class metadata pattern
// CHECK-SAME: @"$s31class_update_callback_with_stub17ResilientSubclassCMP"
// -- metadata completion callback
// CHECK-SAME: @"$s31class_update_callback_with_stub17ResilientSubclassCMr"
// -- method override
// CHECK-SAME: @"got.$s15resilient_class22ResilientOutsideParentCMn"
// CHECK-SAME: @"got.$s15resilient_class22ResilientOutsideParentCACycfCTq"
// CHECK-SAME: @"$s31class_update_callback_with_stub17ResilientSubclassCACycfC"
// -- class stub
// CHECK-SAME: @"$s31class_update_callback_with_stub17ResilientSubclassCMt"
// CHECK-SAME: }>, section "__TEXT,__const", align 4


// -- Symbols for full stubs; the address point is one word in, and defined below

// CHECK-LABEL: @"$s31class_update_callback_with_stub17ResilientSubclassCMt" =
// CHECK-SAME:    internal global %objc_full_class_stub {
// CHECK-SAME:    [[INT]] 0,
// CHECK-SAME:    [[INT]] 1,
// CHECK-SAME:    %objc_class* (%objc_class*, i8*)* @"$s31class_update_callback_with_stub17ResilientSubclassCMU"
// CHECK-SAME:  }

// CHECK-LABEL: @"$s31class_update_callback_with_stub25ResilientNSObjectSubclassCMt" =
// CHECK-SAME:    internal global %objc_full_class_stub {
// CHECK-SAME:    [[INT]] 0,
// CHECK-SAME:    [[INT]] 1,
// CHECK-SAME:    %objc_class* (%objc_class*, i8*)* @"$s31class_update_callback_with_stub25ResilientNSObjectSubclassCMU"
// CHECK-SAME:  }

// CHECK-LABEL: @"$s31class_update_callback_with_stub27FixedLayoutNSObjectSubclassCMt" =
// CHECK-SAME:    internal global %objc_full_class_stub {
// CHECK-SAME:    [[INT]] 0,
// CHECK-SAME:    [[INT]] 1,
// CHECK-SAME:    %objc_class* (%objc_class*, i8*)* @"$s31class_update_callback_with_stub27FixedLayoutNSObjectSubclassCMU"
// CHECK-SAME:  }


// -- Categories reference the stubs

// CHECK-LABEL: @"_CATEGORY__TtC31class_update_callback_with_stub17ResilientSubclass_$_class_update_callback_with_stub" = private constant
// CHECK-SAME:  @"$s31class_update_callback_with_stub17ResilientSubclassCMs"

// CHECK-LABEL: @"_CATEGORY__TtC31class_update_callback_with_stub25ResilientNSObjectSubclass_$_class_update_callback_with_stub" = private constant
// CHECK-SAME:  @"$s31class_update_callback_with_stub25ResilientNSObjectSubclassCMs"

// CHECK-LABEL: @"_CATEGORY__TtC31class_update_callback_with_stub27FixedLayoutNSObjectSubclass_$_class_update_callback_with_stub" = private constant
// CHECK-SAME:  @"$s31class_update_callback_with_stub27FixedLayoutNSObjectSubclassCMs"

// -- But not if the entire inheritance chain is in a single module

// CHECK-LABEL: @"_CATEGORY__TtC15resilient_class22ResilientOutsideParent_$_class_update_callback_with_stub" = private constant
// CHECK-SAME:  @"$s15resilient_class22ResilientOutsideParentCN"


// -- Class stubs do not appear in the class list

// NEGATIVE-NOT: @objc_classes =

// -- The category list

// CHECK-LABEL: @objc_categories = internal global
// CHECK-SAME: @"_CATEGORY__TtC15resilient_class22ResilientOutsideParent_$_class_update_callback_with_stub"
// CHECK-SAME: , section "__DATA,__objc_catlist,regular,no_dead_strip"

// CHECK-LABEL: @objc_categories_stubs = internal global
// CHECK-SAME: @"_CATEGORY__TtC31class_update_callback_with_stub17ResilientSubclass_$_class_update_callback_with_stub"
// CHECK-SAME: @"_CATEGORY__TtC31class_update_callback_with_stub25ResilientNSObjectSubclass_$_class_update_callback_with_stub"
// CHECK-SAME: @"_CATEGORY__TtC31class_update_callback_with_stub27FixedLayoutNSObjectSubclass_$_class_update_callback_with_stub"
// CHECK-SAME: , section "__DATA,__objc_catlist2,regular,no_dead_strip"


// -- Address point for class stubs

// CHECK: @"$s31class_update_callback_with_stub17ResilientSubclassCMs" = alias %objc_class_stub, bitcast (i8* getelementptr inbounds (i8, i8* bitcast (%objc_full_class_stub* @"$s31class_update_callback_with_stub17ResilientSubclassCMt" to i8*), [[INT]] {{4|8}}) to %objc_class_stub*)
// CHECK: @"$s31class_update_callback_with_stub25ResilientNSObjectSubclassCMs" = alias %objc_class_stub, bitcast (i8* getelementptr inbounds (i8, i8* bitcast (%objc_full_class_stub* @"$s31class_update_callback_with_stub25ResilientNSObjectSubclassCMt" to i8*), [[INT]] {{4|8}}) to %objc_class_stub*)


// -- Class symbol for NSObject-derived class points at the class stub
// CHECK: @"OBJC_CLASS_$__TtC31class_update_callback_with_stub25ResilientNSObjectSubclass" = alias %objc_class_stub, %objc_class_stub* @"$s31class_update_callback_with_stub25ResilientNSObjectSubclassCMs"


// -- Metadata update callbacks referenced from class stubs

// CHECK-LABEL: define internal %objc_class* @"$s31class_update_callback_with_stub17ResilientSubclassCMU"(%objc_class*, i8*)
// CHECK:       entry:
// CHECK-NEXT:    [[RESPONSE:%.*]] = call swiftcc %swift.metadata_response @"$s31class_update_callback_with_stub17ResilientSubclassCMa"([[INT]] 0)
// CHECK-NEXT:    [[METADATA:%.*]] = extractvalue %swift.metadata_response [[RESPONSE]], 0
// CHECK-NEXT:    [[CLASS:%.*]] = bitcast %swift.type* [[METADATA]] to %objc_class*
// CHECK-NEXT:    ret %objc_class* [[CLASS]]
// CHECK-NEXT:  }

// CHECK-LABEL: define internal %objc_class* @"$s31class_update_callback_with_stub25ResilientNSObjectSubclassCMU"(%objc_class*, i8*)
// CHECK:       entry:
// CHECK-NEXT:    [[RESPONSE:%.*]] = call swiftcc %swift.metadata_response @"$s31class_update_callback_with_stub25ResilientNSObjectSubclassCMa"([[INT]] 0)
// CHECK-NEXT:    [[METADATA:%.*]] = extractvalue %swift.metadata_response [[RESPONSE]], 0
// CHECK-NEXT:    [[CLASS:%.*]] = bitcast %swift.type* [[METADATA]] to %objc_class*
// CHECK-NEXT:    ret %objc_class* [[CLASS]]
// CHECK-NEXT:  }

open class ResilientSubclass : ResilientOutsideParent {}
open class ResilientNSObjectSubclass : ResilientNSObjectOutsideParent {}

// Note: @_fixed_layout on a class only applies to the storage layout and
// not metadata, which remains resilient.

@_fixed_layout
open class FixedLayoutNSObjectSubclass : FixedLayoutNSObjectOutsideParent {}

extension ResilientSubclass {
  @objc public func objcMethod() {}
}

extension ResilientNSObjectSubclass {
  @objc public func objcMethod() {}
}

extension FixedLayoutNSObjectSubclass {
  @objc public func objcMethod() {}
}

extension ResilientOutsideParent {
  @objc public func anObjcMethod() {}
}
