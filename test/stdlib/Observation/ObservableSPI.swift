// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -swift-version 5 -parse-as-library -plugin-path %swift-plugin-dir -enable-library-evolution -module-name Library -emit-module-path %t/Library.swiftmodule -emit-module-interface-path %t/Library.swiftinterface -emit-private-module-interface-path %t/Library.private.swiftinterface %S/Inputs/ObservableSPI.swift
// RUN: %FileCheck %s --check-prefix=PUBLIC < %t/Library.swiftinterface
// RUN: %FileCheck %s --check-prefix=PRIVATE < %t/Library.private.swiftinterface
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -parse-as-library -I %t
// RUN: %target-swift-frontend -typecheck -parse-as-library -I %t -D SPI %s

// REQUIRES: observation
// REQUIRES: concurrency
// REQUIRES: objc_interop
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// PUBLIC: final public var visible: {{.*}}Int {
// PUBLIC-NOT: hidden

// PRIVATE: @_spi(Testing) final public var hidden: {{.*}}Int {
// PRIVATE-NEXT: @_spi(Testing) get
// PRIVATE-NEXT: @_spi(Testing) set
// PRIVATE: @_spi(Testing) @available
// PRIVATE-NEXT: final public var hiddenAndAvailable: {{.*}}Int {
// PRIVATE-NOT: _hidden

#if SPI
@_spi(Testing) import Library

@available(SwiftStdlib 5.9, *)
func readAndWrite(_ object: ObservableSPI) {
  object.hidden += object.visible
  object.hiddenAndAvailable += 1
}
#else
import Library

@available(SwiftStdlib 5.9, *)
func readAndWrite(_ object: ObservableSPI) {
  object.visible += 1
  object.hidden += 1 // expected-error {{'hidden' is inaccessible due to '@_spi' protection level}}
}
#endif
