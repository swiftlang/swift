// RUN: %target-swift-frontend -typecheck %s -enable-objc-interop -import-objc-header %S/Inputs/enum-anon.h -DDIAGS -verify
// RUN: %target-swift-frontend -emit-ir %s -enable-objc-interop -import-objc-header %S/Inputs/enum-anon.h | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-%target-runtime %s

#if DIAGS
func testDiags() {
  let _: String = Constant2 // expected-error {{cannot convert value of type 'Int' to specified type 'String'}}
  let _: String = VarConstant2 // expected-error {{cannot convert value of type 'UInt32' to specified type 'String'}}

#if _runtime(_ObjC)
  let us2 = USConstant2
#else
  let us2: UInt16 = 0
#endif
  let _: String = us2 // expected-error {{cannot convert value of type 'UInt16' to specified type 'String'}}

#if _runtime(_ObjC)
  let usVar2 = USVarConstant2
#else
  let usVar2: UInt16 = 0
#endif
  let _: String = usVar2 // expected-error {{cannot convert value of type 'UInt16' to specified type 'String'}}

  let _: String = SR2511().y // expected-error {{cannot convert value of type 'UInt32' to specified type 'String'}}

  // The nested anonymous enum value should still have top-level scope, because
  // that's how C works. It should also have the same type as the field (above).
  let _: String = SR2511B // expected-error {{cannot convert value of type 'UInt32' to specified type 'String'}}
  let _: String = SR2511.SR2511B // expected-error {{type 'SR2511' has no member 'SR2511B'}}
}
#endif

// CHECK-LABEL: %TSo6SR2511V = type <{ %Ts5Int32V, %Ts6UInt32V, %Ts5Int32V }>
// CHECK-LABEL: define{{.*}} i32 @"$s4main6testIR1xs5Int32VSPySo6SR2511VG_tF"(
public func testIR(x: UnsafePointer<SR2511>) -> CInt {
  // CHECK: store i32 1, i32* getelementptr inbounds (%Ts6UInt32V, %Ts6UInt32V* bitcast (i32* @global to %Ts6UInt32V*), i32 0, i32 0), align 4
  global = VarConstant2

#if _runtime(_ObjC)
  // CHECK-objc: store i16 1, i16* getelementptr inbounds (%Ts6UInt16V, %Ts6UInt16V* bitcast (i16* @usGlobal to %Ts6UInt16V*), i32 0, i32 0), align 2
  usGlobal = USVarConstant2
#endif

  // Force the definition of the type above.
  // CHECK: ret
  return x.pointee.z
} // CHECK-NEXT: {{^}$}}
