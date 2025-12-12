// RUN: %target-swift-frontend -sil-verify-all -enable-sil-opaque-values -parse-as-library -emit-sil -O %s | %FileCheck %s

// Verify the arguments.  When AccessPathVerification runs, it will be checked
// that the ParamDecl that AddressLowering synthesizes has a specifier
// (ParamSpecifier) set.
// CHECK-LABEL: sil @$s15opaque_values_O3minyxx_xtSLRzlF : {{.*}} {
// CHECK:       bb0(%0 : $*T, %1 : $*T, %2 : $*T):
// CHECK-LABEL: } // end sil function '$s15opaque_values_O3minyxx_xtSLRzlF'
public func min<T: Comparable>(_ x: T, _ y: T) -> T {
  return y < x ? y : x
}


// This example use to produce invalid SIL after RawSILInstLowering of assign_or_init
@propertyWrapper
struct WrapperWithInitialValue<T> {
  var wrappedValue: T
}
public protocol TestProtocol {}
public class TestClass<T> {
  @WrapperWithInitialValue var value: T
  init<U: TestProtocol>(value: T, protocol: U) {
    self.value = value
  }
}
