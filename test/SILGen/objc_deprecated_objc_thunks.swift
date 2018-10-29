// RUN: %target-swift-emit-silgen -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -enable-swift3-objc-inference -swift-version 4 -enable-sil-ownership -enable-objc-interop | %FileCheck -check-prefix CHECK-SWIFT4 %s

import Foundation

class ObjCSubclass : NSObject {
  // CHECK-SWIFT4-LABEL: sil hidden [thunk] @$s016objc_deprecated_A7_thunks12ObjCSubclassC7nothingACyt_tcfcTo : $@convention(objc_method) (@owned ObjCSubclass) -> @owned ObjCSubclass {
  // CHECK-SWIFT4: bb0(%0 : @owned $ObjCSubclass):
  // CHECK-SWIFT4-NEXT: [[FILENAME:%.*]] = [[FILENAME_LITERAL:string_literal.*"]]
  // CHECK-SWIFT4-NEXT: [[LENGTH:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[IS_ASCII:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[LINE:%.*]] = integer_literal $Builtin.Word, [[@LINE+3]]
  // CHECK-SWIFT4-NEXT: [[COLUMN:%.*]] = integer_literal $Builtin.Word, 3
  // CHECK-SWIFT4-NEXT: builtin "swift3ImplicitObjCEntrypoint"([[FILENAME]] : $Builtin.RawPointer, [[LENGTH]] : $Builtin.Word, [[LINE]] : $Builtin.Word, [[COLUMN]] : $Builtin.Word) : $() 
  init(nothing: ()) { super.init() }
  
  // CHECK-SWIFT4-LABEL: sil hidden [thunk] @$s016objc_deprecated_A7_thunks12ObjCSubclassC3fooyyFTo : $@convention(objc_method) (ObjCSubclass) -> ()
  // CHECK-SWIFT4: bb0(%0 : @unowned $ObjCSubclass):
  // CHECK-SWIFT4-NEXT: [[FILENAME:%.*]] = [[FILENAME_LITERAL]]
  // CHECK-SWIFT4-NEXT: [[LENGTH:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[IS_ASCII:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[LINE:%.*]] = integer_literal $Builtin.Word, [[@LINE+3]]
  // CHECK-SWIFT4-NEXT: [[COLUMN:%.*]] = integer_literal $Builtin.Word, 3
  // CHECK-SWIFT4-NEXT: builtin "swift3ImplicitObjCEntrypoint"([[FILENAME]] : $Builtin.RawPointer, [[LENGTH]] : $Builtin.Word, [[LINE]] : $Builtin.Word, [[COLUMN]] : $Builtin.Word) : $() 
  func foo() { }

  // CHECK-SWIFT4-LABEL: sil hidden [thunk] @$s016objc_deprecated_A7_thunks12ObjCSubclassC3barSo8NSObjectCSgvgTo : $@convention(objc_method) (ObjCSubclass) -> @autoreleased Optional<NSObject>
  // CHECK-SWIFT4: bb0(%0 : @unowned $ObjCSubclass):
  // CHECK-SWIFT4-NEXT: [[FILENAME:%.*]] = [[FILENAME_LITERAL]]
  // CHECK-SWIFT4-NEXT: [[LENGTH:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[IS_ASCII:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[LINE:%.*]] = integer_literal $Builtin.Word, [[@LINE+12]]
  // CHECK-SWIFT4-NEXT: [[COLUMN:%.*]] = integer_literal $Builtin.Word, 3
  // CHECK-SWIFT4-NEXT: builtin "swift3ImplicitObjCEntrypoint"([[FILENAME]] : $Builtin.RawPointer, [[LENGTH]] : $Builtin.Word, [[LINE]] : $Builtin.Word, [[COLUMN]] : $Builtin.Word) : $() 

  // CHECK-SWIFT4-LABEL: sil hidden [thunk] @$s016objc_deprecated_A7_thunks12ObjCSubclassC3barSo8NSObjectCSgvsTo : $@convention(objc_method) (Optional<NSObject>, ObjCSubclass) -> () {
  // CHECK-SWIFT4: %0 : @unowned $Optional<NSObject>, %1 : @unowned $ObjCSubclass
  // CHECK-SWIFT4-NEXT: [[FILENAME:%.*]] = [[FILENAME_LITERAL]]
  // CHECK-SWIFT4-NEXT: [[LENGTH:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[IS_ASCII:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[LINE:%.*]] = integer_literal $Builtin.Word, [[@LINE+3]]
  // CHECK-SWIFT4-NEXT: [[COLUMN:%.*]] = integer_literal $Builtin.Word, 3
  // CHECK-SWIFT4-NEXT: builtin "swift3ImplicitObjCEntrypoint"([[FILENAME]] : $Builtin.RawPointer, [[LENGTH]] : $Builtin.Word, [[LINE]] : $Builtin.Word, [[COLUMN]] : $Builtin.Word) : $() 
  var bar: NSObject? = nil

  // CHECK-SWIFT4-LABEL: sil hidden [thunk] @$s016objc_deprecated_A7_thunks12ObjCSubclassCyyXlSicigTo : $@convention(objc_method) (Int, ObjCSubclass) -> @autoreleased AnyObject
  // CHECK-SWIFT4: bb0(%0 : @trivial $Int, %1 : @unowned $ObjCSubclass):
  // CHECK-SWIFT4-NEXT: [[FILENAME:%.*]] = [[FILENAME_LITERAL]]
  // CHECK-SWIFT4-NEXT: [[LENGTH:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[IS_ASCII:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[LINE:%.*]] = integer_literal $Builtin.Word, [[@LINE+12]]
  // CHECK-SWIFT4-NEXT: [[COLUMN:%.*]] = integer_literal $Builtin.Word, 3
  // CHECK-SWIFT4-NEXT: builtin "swift3ImplicitObjCEntrypoint"([[FILENAME]] : $Builtin.RawPointer, [[LENGTH]] : $Builtin.Word, [[LINE]] : $Builtin.Word, [[COLUMN]] : $Builtin.Word) : $() 

  // CHECK-SWIFT4-LABEL: sil hidden [thunk] @$s016objc_deprecated_A7_thunks12ObjCSubclassCyyXlSicisTo : $@convention(objc_method) (AnyObject, Int, ObjCSubclass) ->
  // CHECK-SWIFT4: bb0(%0 : @unowned $AnyObject, %1 : @trivial $Int, %2 : @unowned $ObjCSubclass):
  // CHECK-SWIFT4-NEXT: [[FILENAME:%.*]] = [[FILENAME_LITERAL]]
  // CHECK-SWIFT4-NEXT: [[LENGTH:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[IS_ASCII:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[LINE:%.*]] = integer_literal $Builtin.Word, [[@LINE+3]]
  // CHECK-SWIFT4-NEXT: [[COLUMN:%.*]] = integer_literal $Builtin.Word, 3
  // CHECK-SWIFT4-NEXT: builtin "swift3ImplicitObjCEntrypoint"([[FILENAME]] : $Builtin.RawPointer, [[LENGTH]] : $Builtin.Word, [[LINE]] : $Builtin.Word, [[COLUMN]] : $Builtin.Word) : $() 
  subscript (i: Int) -> AnyObject { get { return self } set { } } 

  // CHECK-SWIFT4-LABEL: sil hidden [thunk] @$s016objc_deprecated_A7_thunks12ObjCSubclassC9staticFooyyFZTo
  // CHECK-SWIFT4: bb0
  // CHECK-SWIFT4-NEXT: [[FILENAME:%.*]] = [[FILENAME_LITERAL]]
  // CHECK-SWIFT4-NEXT: [[LENGTH:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[IS_ASCII:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[LINE:%.*]] = integer_literal $Builtin.Word, [[@LINE+3]]
  // CHECK-SWIFT4-NEXT: [[COLUMN:%.*]] = integer_literal $Builtin.Word, 3
  // CHECK-SWIFT4-NEXT: builtin "swift3ImplicitObjCEntrypoint"([[FILENAME]] : $Builtin.RawPointer, [[LENGTH]] : $Builtin.Word, [[LINE]] : $Builtin.Word, [[COLUMN]] : $Builtin.Word) : $() 
  static func staticFoo() {}

  // CHECK-SWIFT4-LABEL: sil hidden [thunk] [noinline] @$s016objc_deprecated_A7_thunks12ObjCSubclassC13dontInlineFooyyFTo
  // CHECK-SWIFT4: bb0
  // CHECK-SWIFT4-NEXT: [[FILENAME:%.*]] = [[FILENAME_LITERAL]]
  // CHECK-SWIFT4-NEXT: [[LENGTH:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[IS_ASCII:%.*]] = integer_literal
  // CHECK-SWIFT4-NEXT: [[LINE:%.*]] = integer_literal $Builtin.Word, [[@LINE+3]]
  // CHECK-SWIFT4-NEXT: [[COLUMN:%.*]] = integer_literal $Builtin.Word, 3
  // CHECK-SWIFT4-NEXT: builtin "swift3ImplicitObjCEntrypoint"([[FILENAME]] : $Builtin.RawPointer, [[LENGTH]] : $Builtin.Word, [[LINE]] : $Builtin.Word, [[COLUMN]] : $Builtin.Word) : $() 
  @inline(never) func dontInlineFoo() {}
}

extension ObjCSubclass {
	// CHECK-SWIFT4-LABEL: sil hidden [thunk] @$s016objc_deprecated_A7_thunks12ObjCSubclassC13falsePositiveyyFTo : $@convention(objc_method) (ObjCSubclass) -> ()
  // CHECK-SWIFT4: bb0(%0 : @unowned $ObjCSubclass):
  // CHECK-SWIFT4-NOT: builtin "swift3ImplicitObjCEntrypoint"
	// CHECK-SWIFT4: return
  func falsePositive() { }
}

// CHECK-SWIFT3-NOT: builtin "swift3ImplicitObjCEntrypoint"
