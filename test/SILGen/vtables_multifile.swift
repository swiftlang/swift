// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s -primary-file %S/Inputs/vtables_multifile_2.swift | %FileCheck %S/Inputs/vtables_multifile_2.swift

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -enable-library-evolution -emit-module-path %t/vtables_multifile.swiftmodule
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %S/Inputs/vtables_multifile_3.swift -I %t | %FileCheck %S/Inputs/vtables_multifile_3.swift


open class Base<T> {
  fileprivate func privateMethod1() {}
  fileprivate func privateMethod2(_: AnyObject) {}
  fileprivate func privateMethod3(_: Int) {}
  fileprivate func privateMethod4(_: T) {}
}

open class Derived : Base<Int> {
  internal override func privateMethod1() {} // ABI compatible override with same type
  internal override func privateMethod2(_: AnyObject?) {} // ABI compatible override with different type
  internal override func privateMethod3(_: Int?) {} // Requires thunking, different type
  internal override func privateMethod4(_: Int) {} // Requires thunking, same type
}

open class MoreDerived : Derived {
  public override func privateMethod1() {}
  public override func privateMethod2(_: AnyObject?) {}
  public override func privateMethod3(_: Int?) {}
  public override func privateMethod4(_: Int) {}
}

open class MostDerived : MoreDerived {
  open override func privateMethod1() {}
  open override func privateMethod2(_: AnyObject?) {}
  open override func privateMethod3(_: Int?) {}
  open override func privateMethod4(_: Int) {}
}

public final class FinalDerived : Base<Int> {
  internal override func privateMethod1() {}
  internal override func privateMethod2(_: AnyObject?) {}
  internal override func privateMethod3(_: Int?) {}
  internal override func privateMethod4(_: Int) {}
}

// See Inputs/vtables_multifile_2.swift for overrides in a different file.
// See Inputs/vtables_multifile_3.swift for overrides in a different module.

// --
// VTable thunks for the more visible overrides of less visible methods dispatch to the
// vtable slot for the more visible method.
// --

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile7DerivedC14privateMethod1yyFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyFTV : $@convention(method) (@guaranteed Derived) -> () {
// CHECK: bb0(%0 : @guaranteed $Derived):
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %0 : $Derived, #Derived.privateMethod1 : (Derived) -> () -> (), $@convention(method) (@guaranteed Derived) -> ()
// CHECK-NEXT:  apply [[METHOD]](%0) : $@convention(method) (@guaranteed Derived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile7DerivedC14privateMethod2yyyXlSgFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyyXlFTV : $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed Derived) -> () {
// CHECK: bb0(%0 : @guaranteed $Optional<AnyObject>, %1 : @guaranteed $Derived):
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %1 : $Derived, #Derived.privateMethod2 : (Derived) -> (AnyObject?) -> (), $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed Derived) -> ()
// CHECK-NEXT:  apply [[METHOD]](%0, %1) : $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed Derived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile7DerivedC14privateMethod3yySiSgFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyySiFTV : $@convention(method) (Int, @guaranteed Derived) -> () {
// CHECK: bb0(%0 : $Int, %1 : @guaranteed $Derived):
// CHECK-NEXT:  [[ARG:%.*]] = enum $Optional<Int>, #Optional.some!enumelt, %0 : $Int
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %1 : $Derived, #Derived.privateMethod3 : (Derived) -> (Int?) -> (), $@convention(method) (Optional<Int>, @guaranteed Derived) -> ()
// CHECK-NEXT:  apply %3(%2, %1) : $@convention(method) (Optional<Int>, @guaranteed Derived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile7DerivedC14privateMethod4yySiFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyxFTV :
// CHECK: bb0(%0 : $*Int, %1 : @guaranteed $Derived):
// CHECK-NEXT:  [[ARG:%.*]] = load [trivial] %0 : $*Int
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %1 : $Derived, #Derived.privateMethod4 : (Derived) -> (Int) -> (), $@convention(method) (Int, @guaranteed Derived) -> ()
// CHECK-NEXT:  apply %3(%2, %1) : $@convention(method) (Int, @guaranteed Derived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// --
// The subclass can see both the methods of Base and the methods of Derived,
// so it overrides both with thunks that dispatch to methods of MoreDerived.
// --

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile11MoreDerivedC14privateMethod1yyFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyFTV : $@convention(method) (@guaranteed MoreDerived) -> ()
// CHECK: bb0(%0 : @guaranteed $MoreDerived):
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %0 : $MoreDerived, #MoreDerived.privateMethod1 : (MoreDerived) -> () -> (), $@convention(method) (@guaranteed MoreDerived) -> ()
// CHECK-NEXT:  apply [[METHOD]](%0) : $@convention(method) (@guaranteed MoreDerived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile11MoreDerivedC14privateMethod2yyyXlSgFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyyXlFTV : $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed MoreDerived) -> () {
// CHECK: bb0(%0 : @guaranteed $Optional<AnyObject>, %1 : @guaranteed $MoreDerived):
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %1 : $MoreDerived, #MoreDerived.privateMethod2 : (MoreDerived) -> (AnyObject?) -> (), $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed MoreDerived) -> ()
// CHECK-NEXT:  apply [[METHOD]](%0, %1) : $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed MoreDerived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile11MoreDerivedC14privateMethod3yySiSgFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyySiFTV : $@convention(method) (Int, @guaranteed MoreDerived) -> () {
// CHECK: bb0(%0 : $Int, %1 : @guaranteed $MoreDerived):
// CHECK-NEXT:  [[ARG:%.*]] = enum $Optional<Int>, #Optional.some!enumelt, %0 : $Int
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %1 : $MoreDerived, #MoreDerived.privateMethod3 : (MoreDerived) -> (Int?) -> (), $@convention(method) (Optional<Int>, @guaranteed MoreDerived) -> ()
// CHECK-NEXT:  apply %3(%2, %1) : $@convention(method) (Optional<Int>, @guaranteed MoreDerived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile11MoreDerivedC14privateMethod4yySiFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyxFTV :
// CHECK: bb0(%0 : $*Int, %1 : @guaranteed $MoreDerived):
// CHECK-NEXT:  [[ARG:%.*]] = load [trivial] %0 : $*Int
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %1 : $MoreDerived, #MoreDerived.privateMethod4 : (MoreDerived) -> (Int) -> (), $@convention(method) (Int, @guaranteed MoreDerived) -> ()
// CHECK-NEXT:  apply %3(%2, %1) : $@convention(method) (Int, @guaranteed MoreDerived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// --
// Thunks override methods of Derived as well.
// --

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile11MoreDerivedC14privateMethod1yyFAA0D0CADyyFTV : $@convention(method) (@guaranteed MoreDerived) -> () {
// CHECK: bb0(%0 : @guaranteed $MoreDerived):
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %0 : $MoreDerived, #MoreDerived.privateMethod1 : (MoreDerived) -> () -> (), $@convention(method) (@guaranteed MoreDerived) -> ()
// CHECK-NEXT:  apply [[METHOD]](%0) : $@convention(method) (@guaranteed MoreDerived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile11MoreDerivedC14privateMethod2yyyXlSgFAA0D0CADyyAEFTV : $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed MoreDerived) -> () {
// CHECK: bb0(%0 : @guaranteed $Optional<AnyObject>, %1 : @guaranteed $MoreDerived):
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %1 : $MoreDerived, #MoreDerived.privateMethod2 : (MoreDerived) -> (AnyObject?) -> (), $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed MoreDerived) -> ()
// CHECK-NEXT:  apply [[METHOD]](%0, %1) : $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed MoreDerived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile11MoreDerivedC14privateMethod3yySiSgFAA0D0CADyyAEFTV : $@convention(method) (Optional<Int>, @guaranteed MoreDerived) -> () {
// CHECK: bb0(%0 : $Optional<Int>, %1 : @guaranteed $MoreDerived):
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %1 : $MoreDerived, #MoreDerived.privateMethod3 : (MoreDerived) -> (Int?) -> (), $@convention(method) (Optional<Int>, @guaranteed MoreDerived) -> ()
// CHECK-NEXT:  apply [[METHOD]](%0, %1) : $@convention(method) (Optional<Int>, @guaranteed MoreDerived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile11MoreDerivedC14privateMethod4yySiFAA0D0CADyySiFTV : $@convention(method) (Int, @guaranteed MoreDerived) -> () {
// CHECK: bb0(%0 : $Int, %1 : @guaranteed $MoreDerived):
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %1 : $MoreDerived, #MoreDerived.privateMethod4 : (MoreDerived) -> (Int) -> (), $@convention(method) (Int, @guaranteed MoreDerived) -> ()
// CHECK-NEXT:  apply [[METHOD]](%0, %1) : $@convention(method) (Int, @guaranteed MoreDerived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// --
// Thunks for final overrides do not re-dispatch, even if the override is more
// visible.
// --

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile12FinalDerivedC14privateMethod3yySiSgFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyySiFTV : $@convention(method) (Int, @guaranteed FinalDerived) -> () {
// CHECK: bb0(%0 : $Int, %1 : @guaranteed $FinalDerived):
// CHECK-NEXT:  [[ARG:%.*]] = enum $Optional<Int>, #Optional.some!enumelt, %0 : $Int // user: %4
// CHECK:       [[METHOD:%.*]] = function_ref @$s17vtables_multifile12FinalDerivedC14privateMethod3yySiSgF : $@convention(method) (Optional<Int>, @guaranteed FinalDerived) -> ()
// CHECK-NEXT:  apply [[METHOD]]([[ARG]], %1) : $@convention(method) (Optional<Int>, @guaranteed FinalDerived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile12FinalDerivedC14privateMethod4yySiFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyxFTV :
// CHECK: bb0(%0 : $*Int, %1 : @guaranteed $FinalDerived):
// CHECK-NEXT:  [[ARG:%.*]] = load [trivial] %0 : $*Int
// CHECK:       [[METHOD:%.*]] = function_ref @$s17vtables_multifile12FinalDerivedC14privateMethod4yySiF : $@convention(method) (Int, @guaranteed FinalDerived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = apply [[METHOD]]([[ARG]], %1) : $@convention(method) (Int, @guaranteed FinalDerived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }


// --
// VTable for Derived.
// --

// CHECK-LABEL: sil_vtable [serialized] Derived {
// CHECK-NEXT:   #Base.privateMethod1: <T> (Base<T>) -> () -> () : @$s17vtables_multifile7DerivedC14privateMethod1yyFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyFTV [override]  // vtable thunk for Base.privateMethod1() dispatching to Derived.privateMethod1()
// CHECK-NEXT:   #Base.privateMethod2: <T> (Base<T>) -> (AnyObject) -> () : @$s17vtables_multifile7DerivedC14privateMethod2yyyXlSgFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyyXlFTV [override] // vtable thunk for Base.privateMethod2(_:) dispatching to Derived.privateMethod2(_:)
// CHECK-NEXT:   #Base.privateMethod3: <T> (Base<T>) -> (Int) -> () : @$s17vtables_multifile7DerivedC14privateMethod3yySiSgFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyySiFTV [override] // vtable thunk for Base.privateMethod3(_:) dispatching to Derived.privateMethod3(_:)
// CHECK-NEXT:   #Base.privateMethod4: <T> (Base<T>) -> (T) -> () : @$s17vtables_multifile7DerivedC14privateMethod4yySiFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyxFTV [override]      // vtable thunk for Base.privateMethod4(_:) dispatching to Derived.privateMethod4(_:)
// CHECK-NEXT:   #Base.init!allocator: <T> (Base<T>.Type) -> () -> Base<T> : @$s17vtables_multifile7DerivedCACycfC [override]        // Derived.__allocating_init()
// CHECK-NEXT:   #Derived.privateMethod1: (Derived) -> () -> () : @$s17vtables_multifile7DerivedC14privateMethod1yyF // Derived.privateMethod1()
// CHECK-NEXT:   #Derived.privateMethod2: (Derived) -> (AnyObject?) -> () : @$s17vtables_multifile7DerivedC14privateMethod2yyyXlSgF  // Derived.privateMethod2(_:)
// CHECK-NEXT:   #Derived.privateMethod3: (Derived) -> (Int?) -> () : @$s17vtables_multifile7DerivedC14privateMethod3yySiSgF // Derived.privateMethod3(_:)
// CHECK-NEXT:   #Derived.privateMethod4: (Derived) -> (Int) -> () : @$s17vtables_multifile7DerivedC14privateMethod4yySiF    // Derived.privateMethod4(_:)
// CHECK-NEXT:   #Derived.deinit!deallocator: @$s17vtables_multifile7DerivedCfD      // Derived.__deallocating_deinit
// CHECK-NEXT: }

// --
// VTable for MoreDerived.
// --

// CHECK-LABEL: sil_vtable [serialized] MoreDerived {
// CHECK-NEXT:   #Base.privateMethod1: <T> (Base<T>) -> () -> () : @$s17vtables_multifile11MoreDerivedC14privateMethod1yyFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyFTV [override]     // vtable thunk for Base.privateMethod1() dispatching to MoreDerived.privateMethod1()
// CHECK-NEXT:   #Base.privateMethod2: <T> (Base<T>) -> (AnyObject) -> () : @$s17vtables_multifile11MoreDerivedC14privateMethod2yyyXlSgFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyyXlFTV [override]    // vtable thunk for Base.privateMethod2(_:) dispatching to MoreDerived.privateMethod2(_:)
// CHECK-NEXT:   #Base.privateMethod3: <T> (Base<T>) -> (Int) -> () : @$s17vtables_multifile11MoreDerivedC14privateMethod3yySiSgFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyySiFTV [override]    // vtable thunk for Base.privateMethod3(_:) dispatching to MoreDerived.privateMethod3(_:)
// CHECK-NEXT:   #Base.privateMethod4: <T> (Base<T>) -> (T) -> () : @$s17vtables_multifile11MoreDerivedC14privateMethod4yySiFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyxFTV [override] // vtable thunk for Base.privateMethod4(_:) dispatching to MoreDerived.privateMethod4(_:)
// CHECK-NEXT:   #Base.init!allocator: <T> (Base<T>.Type) -> () -> Base<T> : @$s17vtables_multifile11MoreDerivedCACycfC [override]   // MoreDerived.__allocating_init()
// CHECK-NEXT:   #Derived.privateMethod1: (Derived) -> () -> () : @$s17vtables_multifile11MoreDerivedC14privateMethod1yyFAA0D0CADyyFTV [override]     // vtable thunk for Derived.privateMethod1() dispatching to MoreDerived.privateMethod1()
// CHECK-NEXT:   #Derived.privateMethod2: (Derived) -> (AnyObject?) -> () : @$s17vtables_multifile11MoreDerivedC14privateMethod2yyyXlSgFAA0D0CADyyAEFTV [override]    // vtable thunk for Derived.privateMethod2(_:) dispatching to MoreDerived.privateMethod2(_:)
// CHECK-NEXT:   #Derived.privateMethod3: (Derived) -> (Int?) -> () : @$s17vtables_multifile11MoreDerivedC14privateMethod3yySiSgFAA0D0CADyyAEFTV [override]   // vtable thunk for Derived.privateMethod3(_:) dispatching to MoreDerived.privateMethod3(_:)
// CHECK-NEXT:   #Derived.privateMethod4: (Derived) -> (Int) -> () : @$s17vtables_multifile11MoreDerivedC14privateMethod4yySiFAA0D0CADyySiFTV [override]      // vtable thunk for Derived.privateMethod4(_:) dispatching to MoreDerived.privateMethod4(_:)
// CHECK-NEXT:   #MoreDerived.privateMethod1: (MoreDerived) -> () -> () : @$s17vtables_multifile11MoreDerivedC14privateMethod1yyF    // MoreDerived.privateMethod1()
// CHECK-NEXT:   #MoreDerived.privateMethod2: (MoreDerived) -> (AnyObject?) -> () : @$s17vtables_multifile11MoreDerivedC14privateMethod2yyyXlSgF     // MoreDerived.privateMethod2(_:)
// CHECK-NEXT:   #MoreDerived.privateMethod3: (MoreDerived) -> (Int?) -> () : @$s17vtables_multifile11MoreDerivedC14privateMethod3yySiSgF    // MoreDerived.privateMethod3(_:)
// CHECK-NEXT:   #MoreDerived.privateMethod4: (MoreDerived) -> (Int) -> () : @$s17vtables_multifile11MoreDerivedC14privateMethod4yySiF       // MoreDerived.privateMethod4(_:)
// CHECK-NEXT:   #MoreDerived.deinit!deallocator: @$s17vtables_multifile11MoreDerivedCfD     // MoreDerived.__deallocating_deinit
// CHECK-NEXT: }

// --
// MostDerived just makes public methods open, which does not require thunking.
// --

// CHECK-LABEL: sil_vtable [serialized] MostDerived {
// CHECK-NEXT:   #Base.privateMethod1: <T> (Base<T>) -> () -> () : @$s17vtables_multifile11MostDerivedC14privateMethod1yyF
// CHECK-NEXT:   #Base.privateMethod2: <T> (Base<T>) -> (AnyObject) -> () : @$s17vtables_multifile11MostDerivedC14privateMethod2yyyXlSgF
// CHECK-NEXT:   #Base.privateMethod3: <T> (Base<T>) -> (Int) -> () : @$s17vtables_multifile11MostDerivedC14privateMethod3yySiSgF
// CHECK-NEXT:   #Base.privateMethod4: <T> (Base<T>) -> (T) -> () : @$s17vtables_multifile11MostDerivedC14privateMethod4yySiF
// CHECK-NEXT:   #Base.init!allocator: <T> (Base<T>.Type) -> () -> Base<T> : @$s17vtables_multifile11MostDerivedCACycfC [override]   // MostDerived.__allocating_init()
// CHECK-NEXT:   #Derived.privateMethod1: (Derived) -> () -> () : @$s17vtables_multifile11MostDerivedC14privateMethod1yyF
// CHECK-NEXT:   #Derived.privateMethod2: (Derived) -> (AnyObject?) -> () : @$s17vtables_multifile11MostDerivedC14privateMethod2yyyXlSgF
// CHECK-NEXT:   #Derived.privateMethod3: (Derived) -> (Int?) -> () : @$s17vtables_multifile11MostDerivedC14privateMethod3yySiSgF
// CHECK-NEXT:   #Derived.privateMethod4: (Derived) -> (Int) -> () : @$s17vtables_multifile11MostDerivedC14privateMethod4yySiF
// CHECK-NEXT:   #MoreDerived.privateMethod1: (MoreDerived) -> () -> () : @$s17vtables_multifile11MostDerivedC14privateMethod1yyF [override] // MostDerived.privateMethod1()
// CHECK-NEXT:   #MoreDerived.privateMethod2: (MoreDerived) -> (AnyObject?) -> () : @$s17vtables_multifile11MostDerivedC14privateMethod2yyyXlSgF [override] // MostDerived.privateMethod2(_:)
// CHECK-NEXT:   #MoreDerived.privateMethod3: (MoreDerived) -> (Int?) -> () : @$s17vtables_multifile11MostDerivedC14privateMethod3yySiSgF [override] // MostDerived.privateMethod3(_:)
// CHECK-NEXT:   #MoreDerived.privateMethod4: (MoreDerived) -> (Int) -> () : @$s17vtables_multifile11MostDerivedC14privateMethod4yySiF [override] // MostDerived.privateMethod4(_:)
// CHECK-NEXT:   #MostDerived.deinit!deallocator: @$s17vtables_multifile11MostDerivedCfD     // MostDerived.__deallocating_deinit
// CHECK-NEXT: }

// --
// FinalDerived adds a final override; make sure we handle this correctly.
// --

// CHECK-LABEL: sil_vtable [serialized] FinalDerived {
// CHECK-NEXT:   #Base.privateMethod1: <T> (Base<T>) -> () -> () : @$s17vtables_multifile12FinalDerivedC14privateMethod1yyF [override]	// FinalDerived.privateMethod1()
// CHECK-NEXT:   #Base.privateMethod2: <T> (Base<T>) -> (AnyObject) -> () : @$s17vtables_multifile12FinalDerivedC14privateMethod2yyyXlSgF [override]	// FinalDerived.privateMethod2(_:)
// CHECK-NEXT:   #Base.privateMethod3: <T> (Base<T>) -> (Int) -> () : @$s17vtables_multifile12FinalDerivedC14privateMethod3yySiSgFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyySiFTV [override]	// vtable thunk for Base.privateMethod3(_:) dispatching to FinalDerived.privateMethod3(_:)
// CHECK-NEXT:   #Base.privateMethod4: <T> (Base<T>) -> (T) -> () : @$s17vtables_multifile12FinalDerivedC14privateMethod4yySiFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyxFTV [override]	// vtable thunk for Base.privateMethod4(_:) dispatching to FinalDerived.privateMethod4(_:)
// CHECK-NEXT:   #Base.init!allocator: <T> (Base<T>.Type) -> () -> Base<T> : @$s17vtables_multifile12FinalDerivedCACycfC [override]	// FinalDerived.__allocating_init()
// CHECK-NEXT:   #FinalDerived.deinit!deallocator: @$s17vtables_multifile12FinalDerivedCfD	// FinalDerived.__deallocating_deinit
// CHECK-NEXT: }
