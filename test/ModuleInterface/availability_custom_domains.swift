// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/Test.swiftinterface) %s \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -module-name Test

// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -module-name Test

// RUN: %FileCheck %s < %t/Test.swiftinterface

// REQUIRES: swift_feature_CustomAvailability

import Oceans // re-exports Rivers

// CHECK-NOT:  $CustomAvailability

// CHECK:      @available(Colorado)
// CHECK-NEXT: public func availableInColorado()
@available(Colorado)
public func availableInColorado() { }

// CHECK:      @available(Arctic, unavailable)
// CHECK-NEXT: @available(Pacific)
// CHECK-NEXT: public func unavailableInArcticButAvailableInPacific()
@available(Arctic, unavailable)
@available(Pacific)
public func unavailableInArcticButAvailableInPacific() { }

// CHECK:      @available(Colorado)
// CHECK-NEXT: @available(Pacific)
// CHECK-NEXT: @_Concurrency::MainActor public class AvailableInColoradoAndPacificMainActorClass
@available(Colorado)
@available(Pacific)
@MainActor
public class AvailableInColoradoAndPacificMainActorClass { }

// CHECK:      @available(Colorado)
// CHECK-NEXT: @available(Pacific)
// CHECK-NEXT: public enum AvailableInColoradoAndPacificEquatableEnum
@available(Colorado)
@available(Pacific)
public enum AvailableInColoradoAndPacificEquatableEnum {
  case a
}

// CHECK:      @available(Colorado)
// CHECK-NEXT: public struct NonSendableAvailableInColorado
@available(Colorado)
@_nonSendable
public struct NonSendableAvailableInColorado { }

// CHECK:      @available(Arctic, unavailable)
// CHECK-NEXT: public struct NonSendableUnavailableInArctic
@available(Arctic, unavailable)
@_nonSendable
public struct NonSendableUnavailableInArctic { }

// CHECK:      @available(Colorado)
// CHECK-NEXT: @available(Pacific)
// CHECK-NEXT: public struct NonSendableAvailableInColoradoAndPacific
@available(Colorado)
@available(Pacific)
@_nonSendable
public struct NonSendableAvailableInColoradoAndPacific { }

// CHECK:      @available(Pacific)
// CHECK-NEXT: public struct AvailableInPacificOuter
@available(Pacific)
public struct AvailableInPacificOuter {
  @_nonSendable
  public struct NonSendableNested { }
}

// CHECK:      @available(Colorado)
// CHECK-NEXT: @available(*, unavailable)
// CHECK-NEXT: extension Test::NonSendableAvailableInColorado : @unchecked Swift::Sendable {

// CHECK:      @available(Arctic, unavailable)
// CHECK-NEXT: @available(*, unavailable)
// CHECK-NEXT: extension Test::NonSendableUnavailableInArctic : @unchecked Swift::Sendable {

// CHECK:      @available(Pacific)
// CHECK-NEXT: @available(Colorado)
// CHECK-NEXT: @available(*, unavailable)
// CHECK-NEXT: extension Test::NonSendableAvailableInColoradoAndPacific : @unchecked Swift::Sendable {

// CHECK:      @available(Pacific)
// CHECK-NEXT: @available(*, unavailable)
// CHECK-NEXT: extension Test::AvailableInPacificOuter.Test::NonSendableNested : @unchecked Swift::Sendable {

// CHECK:      @available(Colorado)
// CHECK-NEXT: @available(Pacific)
// CHECK-NEXT: extension Test::AvailableInColoradoAndPacificMainActorClass : Swift::Sendable {}

// CHECK:      @available(Colorado)
// CHECK-NEXT: @available(Pacific)
// CHECK-NEXT: extension Test::AvailableInColoradoAndPacificEquatableEnum : Swift::Equatable {}

// CHECK:      @available(Colorado)
// CHECK-NEXT: @available(Pacific)
// CHECK-NEXT: extension Test::AvailableInColoradoAndPacificEquatableEnum : Swift::Hashable {}

