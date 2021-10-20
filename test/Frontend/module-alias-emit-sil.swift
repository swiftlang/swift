/// Test emit sil with module aliasing.
///
/// Module 'Lib' imports module 'XLogging', and 'XLogging' is aliased 'AppleLogging'.

// REQUIRES: rdar84436250

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// Create AppleLogging.swiftmodule by aliasing XLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias XLogging=AppleLogging %t/FileLogging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule

/// Verify generated SIL only contains AppleLogging
// RUN: %target-swift-frontend -emit-sil %t/FileLib.swift -module-alias XLogging=AppleLogging -I %t > %t/result-sil.output
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

// CHECK: public func start() -> Loggable?

// CHECK: public func end(_ arg: Logger)

// CHECK: // main
// CHECK: sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
// CHECK: bb0(%0 : $Int32, %1 : $UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>):
// CHECK:   %2 = integer_literal $Builtin.Int32, 0          // user: %3
// CHECK:   %3 = struct $Int32 (%2 : $Builtin.Int32)        // user: %4
// CHECK:   return %3 : $Int32                              // id: %4
// CHECK:  // end sil function 'main'

// CHECK: // MyLib.verbosity.getter
// CHECK: sil @$s7FileLib02MyB0V9verbositySivg : $@convention(method) (MyLib) -> Int {
// CHECK: // %0 "self"                                      // user: %1
// CHECK: bb0(%0 : $MyLib):
// CHECK:   debug_value %0 : $MyLib, let, name "self", argno 1, implicit // id: %1
// CHECK:   %2 = integer_literal $Builtin.Int64, 3          // user: %3
// CHECK:   %3 = struct $Int (%2 : $Builtin.Int64)          // user: %4
// CHECK:   return %3 : $Int                                // id: %4
// CHECK: } // end sil function '$s7FileLib02MyB0V9verbositySivg'

// CHECK: // Int.init(_builtinIntegerLiteral:)
// CHECK: sil public_external [transparent] @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int {
// CHECK: // %0                                             // user: %2
// CHECK: bb0(%0 : $Builtin.IntLiteral, %1 : $@thin Int.Type):
// CHECK:   %2 = builtin "s_to_s_checked_trunc_IntLiteral_Int64"(%0 : $Builtin.IntLiteral) : $(Builtin.Int64, Builtin.Int1) // user: %3
// CHECK:   %3 = tuple_extract %2 : $(Builtin.Int64, Builtin.Int1), 0 // user: %4
// CHECK:   %4 = struct $Int (%3 : $Builtin.Int64)          // user: %5
// CHECK:   return %4 : $Int                                // id: %5
// CHECK: } // end sil function '$sSi22_builtinIntegerLiteralSiBI_tcfC'

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
// CHECK: sil @$s7FileLib5start12AppleLogging8Loggable_pSgyF : $@convention(thin) () -> @out Optional<Loggable> {
// CHECK: // %0 "$return_value"                             // user: %2
// CHECK: bb0(%0 : $*Optional<Loggable>):
// CHECK:   // function_ref setup()
// CHECK:   %1 = function_ref @$s12AppleLogging5setupAA8Loggable_pSgyF : $@convention(thin) () -> @out Optional<Loggable> // user: %2
// CHECK:   %2 = apply %1(%0) : $@convention(thin) () -> @out Optional<Loggable>
// CHECK:   %3 = tuple ()                                   // user: %4
// CHECK: return %3 : $()                                 // id: %4
// CHECK: } // end sil function '$s7FileLib5start12AppleLogging8Loggable_pSgyF'

// CHECK: // setup()
// CHECK: sil @$s12AppleLogging5setupAA8Loggable_pSgyF : $@convention(thin) () -> @out Optional<Loggable>

// CHECK: // end(_:)
// CHECK: sil @$s7FileLib3endyy12AppleLogging6LoggerVF : $@convention(thin) (Logger) -> () {
// CHECK: // %0 "arg"                                       // user: %1
// CHECK: bb0(%0 : $Logger):
// CHECK:   debug_value %0 : $Logger, let, name "arg", argno 1 // id: %1
// CHECK:   %2 = tuple ()                                   // user: %3
// CHECK:   return %2 : $()                                 // id: %3
// CHECK: } // end sil function '$s7FileLib3endyy12AppleLogging6LoggerVF'

// CHECK: sil_witness_table MyLib: Loggable module FileLib {
// CHECK:   method #Loggable.verbosity!getter: <Self where Self : Loggable> (Self) -> () -> Int : @$s7FileLib02MyB0V12AppleLogging8LoggableAadEP9verbositySivgTW  // protocol witness for Loggable.verbosity.getter in conformance MyLib
// CHECK: }

// CHECK: sil_property #MyLib.verbosity ()



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
