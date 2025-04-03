// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -module-name Horse -D LIB -emit-module-path %t/Horse.swiftmodule
// RUN: %target-swift-emit-silgen %s -module-name main -I %t -module-alias SwiftHorse=Horse | %FileCheck %s

#if LIB
  public protocol Equine {}
#else
  import SwiftHorse

  // Make sure we use the module's real name, and not its alias name, so that
  // we always have Horse.Equine < Swift.Equatable. If this doesn't hold, the
  // two requirements in the mangling will be flipped.

  // CHECK-LABEL: sil hidden [ossa] @$s4main21requirementOrderHorseyyx0D06EquineRzSQRzlF : $@convention(thin) <T where T : Equine, T : Equatable> (@in_guaranteed T) -> () {
  func requirementOrderHorse<T: Equine & Equatable>(_: T) {}
#endif