/// Test emit sil with module aliasing.
///
/// Module 'Lib' imports module 'XLogging', and 'XLogging' is aliased 'AppleLogging'.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// Create AppleLogging.swiftmodule by aliasing XLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias XLogging=AppleLogging %t/FileLogging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule

/// Verify generated SIL only contains AppleLogging
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil %t/FileLib.swift -module-alias XLogging=AppleLogging -I %t > %t/result-sil.output
// RUN: %FileCheck %s -input-file %t/result-sil.output
// RUN: not %FileCheck %s -input-file %t/result-sil.output -check-prefix CHECK-NOT-FOUND
// CHECK-NOT-FOUND: XLogging

// CHECK: sil_stage canonical

// CHECK: import Builtin
// CHECK: import Swift
// CHECK: import SwiftShims

// CHECK: import AppleLogging

// CHECK: public struct MyLib : Loggable {
// CHECK:   public var verbosity: Int { get }
// CHECK:   init()
// CHECK: }

// CHECK: public func start() -> (any Loggable)?

// CHECK: public func end(_ arg: Logger)

// CHECK: // protocol witness for Loggable.verbosity.getter in conformance MyLib
// CHECK: sil shared [transparent] [thunk] @$s7FileLib02MyB0V12AppleLogging8LoggableAadEP9verbositySivgTW : $@convention(witness_method: Loggable) (@in_guaranteed MyLib) -> Int {
// CHECK: // %0                                             // user: %1
// CHECK: bb0(%0 : $*MyLib):
// CHECK:   %1 = load %0 : $*MyLib                          // user: %3
// CHECK:   // function_ref MyLib.verbosity.getter
// CHECK:   %2 = function_ref @$s7FileLib02MyB0V9verbositySivg : $@convention(method) (MyLib) -> Int // user: %3
// CHECK:   %3 = apply %2(%1) : $@convention(method) (MyLib) -> Int // user: %4
// CHECK:   return %3 : $Int                                // id: %4
// CHECK: } // end sil function '$s7FileLib02MyB0V12AppleLogging8LoggableAadEP9verbositySivgTW'

// CHECK: // start()
// CHECK: sil @$s7FileLib5start12AppleLogging8Loggable_pSgyF : $@convention(thin) () -> @out Optional<any Loggable> {
// CHECK: // %0 "$return_value"                             // user: %2
// CHECK: bb0(%0 : $*Optional<any Loggable>):
// CHECK:   // function_ref setup()
// CHECK:   %1 = function_ref @$s12AppleLogging5setupAA8Loggable_pSgyF : $@convention(thin) () -> @out Optional<any Loggable> // user: %2
// CHECK:   %2 = apply %1(%0) : $@convention(thin) () -> @out Optional<any Loggable>
// CHECK:   %3 = tuple ()                                   // user: %4
// CHECK: return %3 : $()                                 // id: %4
// CHECK: } // end sil function '$s7FileLib5start12AppleLogging8Loggable_pSgyF'

// CHECK: // setup()
// CHECK: sil @$s12AppleLogging5setupAA8Loggable_pSgyF : $@convention(thin) () -> @out Optional<any Loggable>

// CHECK: // end(_:)
// CHECK: sil @$s7FileLib3endyy12AppleLogging6LoggerVF : $@convention(thin) (Logger) -> () {
// CHECK: } // end sil function '$s7FileLib3endyy12AppleLogging6LoggerVF'



// BEGIN FileLogging.swift
public protocol Loggable {
  var verbosity: Int { get }
}
public struct Logger {
  public init() {}
}
public func setup() -> XLogging.Loggable? {
  return nil
}

// BEGIN FileLib.swift
import XLogging

public struct MyLib: Loggable {
  public var verbosity: Int {
    return 3
  }
}
public func start() -> XLogging.Loggable? {
  return XLogging.setup()
}

public func end(_ arg: XLogging.Logger) {
}
