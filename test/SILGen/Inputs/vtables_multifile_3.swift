import vtables_multifile

open class OtherDerived : MostDerived {
  open override func privateMethod1() {
    super.privateMethod1()
  }
  open override func privateMethod2(_ arg: AnyObject?) {
    super.privateMethod2(arg)
  }
  open override func privateMethod3(_ arg: Int?) {
    super.privateMethod3(arg)
  }
  open override func privateMethod4(_ arg: Int) {
    super.privateMethod4(arg)
  }
}

// Note that the vtable does not mention the private methods of Base
// or Derived, which we cannot see from here.

// CHECK-LABEL: sil_vtable [serialized] OtherDerived {
// CHECK-NEXT:   #MoreDerived.privateMethod1: (MoreDerived) -> () -> () : @$s19vtables_multifile_312OtherDerivedC14privateMethod1yyF [override]      // OtherDerived.privateMethod1()
// CHECK-NEXT:   #MoreDerived.privateMethod2: (MoreDerived) -> (AnyObject?) -> () : @$s19vtables_multifile_312OtherDerivedC14privateMethod2yyyXlSgF [override]       // OtherDerived.privateMethod2(_:)
// CHECK-NEXT:   #MoreDerived.privateMethod3: (MoreDerived) -> (Int?) -> () : @$s19vtables_multifile_312OtherDerivedC14privateMethod3yySiSgF [override]      // OtherDerived.privateMethod3(_:)
// CHECK-NEXT:   #MoreDerived.privateMethod4: (MoreDerived) -> (Int) -> () : @$s19vtables_multifile_312OtherDerivedC14privateMethod4yySiF [override] // OtherDerived.privateMethod4(_:)
// CHECK-NEXT:   #OtherDerived.deinit!deallocator: @$s19vtables_multifile_312OtherDerivedCfD // OtherDerived.__deallocating_deinit
// CHECK-NEXT: }
