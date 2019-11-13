open class OtherDerived : Derived {
  internal override func privateMethod1() {
    super.privateMethod1()
  }
  internal override func privateMethod2(_ arg: AnyObject?) {
    super.privateMethod2(arg)
  }
  internal override func privateMethod3(_ arg: Int?) {
    super.privateMethod3(arg)
  }
  internal override func privateMethod4(_ arg: Int) {
    super.privateMethod4(arg)
  }
}

// --
// Super method calls directly reference the superclass method
// --

// CHECK-LABEL: sil hidden [ossa] @$s17vtables_multifile12OtherDerivedC14privateMethod1yyF : $@convention(method) (@guaranteed OtherDerived) -> () {
// CHECK: bb0(%0 : @guaranteed $OtherDerived):
// CHECK:       [[SELF:%.*]] = copy_value %0 : $OtherDerived
// CHECK-NEXT:  [[SUPER:%.*]] = upcast [[SELF]] : $OtherDerived to $Derived
// CHECK:       [[METHOD:%.*]] = function_ref @$s17vtables_multifile7DerivedC14privateMethod1yyF : $@convention(method) (@guaranteed Derived) -> () // user: %5
// CHECK-NEXT:  apply [[METHOD:%.*]]([[SUPER]]) : $@convention(method) (@guaranteed Derived) -> ()
// CHECK-NEXT:  destroy_value [[SUPER]] : $Derived
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT:}

// CHECK-LABEL: sil hidden_external @$s17vtables_multifile7DerivedC14privateMethod1yyF : $@convention(method) (@guaranteed Derived) -> ()

// CHECK-LABEL: sil hidden [ossa] @$s17vtables_multifile12OtherDerivedC14privateMethod2yyyXlSgF : $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed OtherDerived) -> () {
// CHECK: bb0(%0 : @guaranteed $Optional<AnyObject>, %1 : @guaranteed $OtherDerived):
// CHECK:       [[SELF:%.*]] = copy_value %1 : $OtherDerived
// CHECK-NEXT:  [[SUPER:%.*]] = upcast [[SELF]] : $OtherDerived to $Derived
// CHECK:       [[METHOD:%.*]] = function_ref @$s17vtables_multifile7DerivedC14privateMethod2yyyXlSgF : $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed Derived) -> ()
// CHECK-NEXT:  apply [[METHOD]](%0, [[SUPER]]) : $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed Derived) -> ()
// CHECK-NEXT:  destroy_value [[SUPER]] : $Derived
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil hidden_external @$s17vtables_multifile7DerivedC14privateMethod2yyyXlSgF : $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed Derived) -> ()
// CHECK: bb0(%0 : $Optional<Int>, %1 : @guaranteed $OtherDerived):
// CHECK:       [[SELF:%.*]] = copy_value %1 : $OtherDerived
// CHECK-NEXT:  [[SUPER:%.*]] = upcast [[SELF]] : $OtherDerived to $Derived
// CHECK:       [[METHOD:%.*]] = function_ref @$s17vtables_multifile7DerivedC14privateMethod3yySiSgF : $@convention(method) (Optional<Int>, @guaranteed Derived) -> ()
// CHECK-NEXT:  apply [[METHOD]](%0, [[SUPER]]) : $@convention(method) (Optional<Int>, @guaranteed Derived) -> ()
// CHECK-NEXT:  destroy_value [[SUPER]] : $Derived
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil hidden_external @$s17vtables_multifile7DerivedC14privateMethod3yySiSgF : $@convention(method) (Optional<Int>, @guaranteed Derived) -> ()
// CHECK: bb0(%0 : $Int, %1 : @guaranteed $OtherDerived):
// CHECK:       [[SELF:%.*]] = copy_value %1 : $OtherDerived
// CHECK-NEXT:  [[SUPER:%.*]] = upcast [[SELF]] : $OtherDerived to $Derived
// CHECK:       [[METHOD:%.*]] = function_ref @$s17vtables_multifile7DerivedC14privateMethod4yySiF : $@convention(method) (Int, @guaranteed Derived) -> ()
// CHECK-NEXT:  apply [[METHOD]](%0, [[SUPER]]) : $@convention(method) (Int, @guaranteed Derived) -> ()
// CHECK-NEXT:  destroy_value [[SUPER]] : $Derived
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil hidden_external @$s17vtables_multifile7DerivedC14privateMethod4yySiF : $@convention(method) (Int, @guaranteed Derived) -> ()

// --
// VTable thunks for methods of Base redispatch to methods of Derived
// --

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile7DerivedC14privateMethod1yyFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyFTV : $@convention(method) (@guaranteed Derived) -> () {
// CHECK: bb0(%0 : @guaranteed $Derived):
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %0 : $Derived, #Derived.privateMethod1!1 : (Derived) -> () -> (), $@convention(method) (@guaranteed Derived) -> ()
// CHECK-NEXT:  apply [[METHOD]](%0) : $@convention(method) (@guaranteed Derived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile7DerivedC14privateMethod2yyyXlSgFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyyXlFTV : $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed Derived) -> () {
// CHECK: bb0(%0 : @guaranteed $Optional<AnyObject>, %1 : @guaranteed $Derived):
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %1 : $Derived, #Derived.privateMethod2!1 : (Derived) -> (AnyObject?) -> (), $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed Derived) -> ()
// CHECK-NEXT:  apply [[METHOD]](%0, %1) : $@convention(method) (@guaranteed Optional<AnyObject>, @guaranteed Derived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile7DerivedC14privateMethod3yySiSgFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyySiFTV : $@convention(method) (Int, @guaranteed Derived) -> () {
// CHECK: bb0(%0 : $Int, %1 : @guaranteed $Derived):
// CHECK-NEXT:  [[ARG:%.*]] = enum $Optional<Int>, #Optional.some!enumelt.1, %0 : $Int
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %1 : $Derived, #Derived.privateMethod3!1 : (Derived) -> (Int?) -> (), $@convention(method) (Optional<Int>, @guaranteed Derived) -> ()
// CHECK-NEXT:  apply %3(%2, %1) : $@convention(method) (Optional<Int>, @guaranteed Derived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil private [thunk] [ossa] @$s17vtables_multifile7DerivedC14privateMethod4yySiFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyxFTV : $@convention(method) (@in_guaranteed Int, @guaranteed Derived) -> () {
// CHECK: bb0(%0 : $*Int, %1 : @guaranteed $Derived):
// CHECK-NEXT:  [[ARG:%.*]] = load [trivial] %0 : $*Int
// CHECK-NEXT:  [[METHOD:%.*]] = class_method %1 : $Derived, #Derived.privateMethod4!1 : (Derived) -> (Int) -> (), $@convention(method) (Int, @guaranteed Derived) -> ()
// CHECK-NEXT:  apply %3(%2, %1) : $@convention(method) (Int, @guaranteed Derived) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable [serialized] OtherDerived {
// CHECK-NEXT:  #Base.privateMethod1!1: <T> (Base<T>) -> () -> () : @$s17vtables_multifile7DerivedC14privateMethod1yyFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyFTV [inherited] // vtable thunk for Base.privateMethod1() dispatching to Derived.privateMethod1()
// CHECK-NEXT:  #Base.privateMethod2!1: <T> (Base<T>) -> (AnyObject) -> () : @$s17vtables_multifile7DerivedC14privateMethod2yyyXlSgFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyyXlFTV [inherited]        // vtable thunk for Base.privateMethod2(_:) dispatching to Derived.privateMethod2(_:)
// CHECK-NEXT:  #Base.privateMethod3!1: <T> (Base<T>) -> (Int) -> () : @$s17vtables_multifile7DerivedC14privateMethod3yySiSgFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyySiFTV [inherited]        // vtable thunk for Base.privateMethod3(_:) dispatching to Derived.privateMethod3(_:)
// CHECK-NEXT:  #Base.privateMethod4!1: <T> (Base<T>) -> (T) -> () : @$s17vtables_multifile7DerivedC14privateMethod4yySiFAA4BaseCAD33_63E5F2521A3C787F5F9EFD57FB9237EALLyyxFTV [inherited]     // vtable thunk for Base.privateMethod4(_:) dispatching to Derived.privateMethod4(_:)
// CHECK-NEXT:  #Base.init!allocator.1: <T> (Base<T>.Type) -> () -> Base<T> : @$s17vtables_multifile12OtherDerivedCACycfC [override]  // OtherDerived.__allocating_init()
// CHECK-NEXT:  #Derived.privateMethod1!1: (Derived) -> () -> () : @$s17vtables_multifile12OtherDerivedC14privateMethod1yyF [override]        // OtherDerived.privateMethod1()
// CHECK-NEXT:  #Derived.privateMethod2!1: (Derived) -> (AnyObject?) -> () : @$s17vtables_multifile12OtherDerivedC14privateMethod2yyyXlSgF [override] // OtherDerived.privateMethod2(_:)
// CHECK-NEXT:  #Derived.privateMethod3!1: (Derived) -> (Int?) -> () : @$s17vtables_multifile12OtherDerivedC14privateMethod3yySiSgF [override]        // OtherDerived.privateMethod3(_:)
// CHECK-NEXT:  #Derived.privateMethod4!1: (Derived) -> (Int) -> () : @$s17vtables_multifile12OtherDerivedC14privateMethod4yySiF [override]   // OtherDerived.privateMethod4(_:)
// CHECK-NEXT:  #OtherDerived.deinit!deallocator.1: @$s17vtables_multifile12OtherDerivedCfD   // OtherDerived.__deallocating_deinit
// CHECK-NEXT:}
