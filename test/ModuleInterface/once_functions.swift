// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -parse-as-library -language-mode 5 -enable-library-evolution -enable-experimental-feature OnceFunctions -module-name OnceFunctions -emit-module-interface-path %t/calledonce.swiftinterface %t/OnceFunctions.swift
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/calledonce.swiftinterface -module-name OnceFunctions
// RUN: %FileCheck %s --input-file %t/calledonce.swiftinterface

// A client sees the same parameter ownership whether it imports the module
// built from source or the one built from the interface.
// RUN: %empty-directory(%t/FromSource)
// RUN: %empty-directory(%t/FromInterface)
// RUN: %target-swift-frontend -emit-module -parse-as-library -language-mode 5 -enable-library-evolution -enable-experimental-feature OnceFunctions -experimental-skip-non-inlinable-function-bodies -module-name OnceFunctions -o %t/FromSource/OnceFunctions.swiftmodule %t/OnceFunctions.swift
// RUN: %target-swift-frontend -compile-module-from-interface %t/calledonce.swiftinterface -module-name OnceFunctions -o %t/FromInterface/OnceFunctions.swiftmodule
// RUN: %target-swift-frontend -emit-silgen -enable-experimental-feature OnceFunctions -module-name Client -I %t/FromSource %t/Client.swift -o %t/FromSource.sil
// RUN: %target-swift-frontend -emit-silgen -enable-experimental-feature OnceFunctions -module-name Client -I %t/FromInterface %t/Client.swift -o %t/FromInterface.sil
// RUN: diff %t/FromSource.sil %t/FromInterface.sil
// RUN: %FileCheck %s --check-prefix=CLIENT --input-file %t/FromInterface.sil

// REQUIRES: swift_feature_OnceFunctions

//--- OnceFunctions.swift

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public typealias FnType = @callable(once) () -> ()
// CHECK: #endif
public typealias FnType = @callable(once) () -> ()

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func test1(_: consuming @callable(once) () -> ())
// CHECK: #endif
public func test1(_: @callable(once) () -> ()) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func test2(_: consuming @autoclosure @callable(once) () -> ())
// CHECK: #endif
public func test2(_: @autoclosure @callable(once) () -> ()) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func test3(_: () -> @callable(once) () -> Swift::Void)
// CHECK: #endif
public func test3(_: () -> @callable(once) () -> Void) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func test4(_: consuming @escaping @callable(once) () -> ())
// CHECK: #endif
public func test4(_: @escaping @callable(once) () -> ()) {}

public struct Test: ~Copyable {
  // CHECK: #if compiler(>=5.3) && $OnceFunctions
  // CHECK: public let prop: (@callable(once) () -> Swift::Void)?
  // CHECK: #endif
  public let prop: (@callable(once) () -> Void)? = nil

  // CHECK: #if compiler(>=5.3) && $OnceFunctions
  // CHECK: public func f(_: (consuming @callable(once) () -> Swift::Void) -> Swift::Void)
  // CHECK: #endif
  public func f(_: (@callable(once) () -> Void) -> Void) {}
}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public typealias ExactlyOnceFnType = @once () -> ()
// CHECK: #endif
public typealias ExactlyOnceFnType = @once () -> ()

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func exactlyOnce1(_: consuming @once () -> ())
// CHECK: #endif
public func exactlyOnce1(_: @once () -> ()) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func exactlyOnce2(_: consuming @autoclosure @once () -> ())
// CHECK: #endif
public func exactlyOnce2(_: @autoclosure @once () -> ()) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func exactlyOnce3(_: () -> @once () -> Swift::Void)
// CHECK: #endif
public func exactlyOnce3(_: () -> @once () -> Void) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func exactlyOnce4(_: consuming @escaping @once () -> ())
// CHECK: #endif
public func exactlyOnce4(_: @escaping @once () -> ()) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func exactlyOnce5(_: (consuming @once () -> Swift::Void) -> Swift::Void)
// CHECK: #endif
public func exactlyOnce5(_: (@once () -> Void) -> Void) {}

// An explicit specifier is printed.

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func explicitConsuming(_: consuming @callable(once) () -> ())
// CHECK: #endif
public func explicitConsuming(_: consuming @callable(once) () -> ()) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func explicitInOut(_: inout @callable(once) () -> ())
// CHECK: #endif
public func explicitInOut(_: inout @callable(once) () -> ()) {}

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func explicitOwned(_: __owned @once () -> ())
// CHECK: #endif
public func explicitOwned(_: __owned @once () -> ()) {}

// An optional parameter isn't consuming unless it's written as such.

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func optionalConsuming(_: consuming (@callable(once) () -> ())?)
// CHECK: #endif
public func optionalConsuming(_: consuming (@callable(once) () -> ())?) {}

// A typealias doesn't make a parameter consuming.

// CHECK: public func aliasDefault(_: OnceFunctions::FnType)
public func aliasDefault(_: FnType) {}

// CHECK: public func aliasConsuming(_: consuming OnceFunctions::FnType)
public func aliasConsuming(_: consuming FnType) {}

// CHECK: @inlinable public func inlinableAliasConsuming(_ x: consuming OnceFunctions::FnType) { x() }
@inlinable public func inlinableAliasConsuming(_ x: consuming FnType) { x() }

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: @inlinable public func inlinableExplicitConsuming(_ x: consuming @callable(once) () -> ()) { x() }
// CHECK: #endif
@inlinable public func inlinableExplicitConsuming(
  _ x: consuming @callable(once) () -> ()
) { x() }

// A `sending` parameter isn't consuming unless it's written as such.

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func sendingConsuming(_: consuming sending @escaping @callable(once) () -> ())
// CHECK: #endif
public func sendingConsuming(
  _: consuming sending @escaping @callable(once) () -> ()
) {}

// A parameter of a function type is consuming when its type is a typealias
// too.

// CHECK: #if compiler(>=5.3) && $OnceFunctions
// CHECK: public func explicitConsumingInFunctionType(_: (consuming @callable(once) () -> Swift::Void) -> Swift::Void)
// CHECK: #endif
public func explicitConsumingInFunctionType(
  _: (consuming @callable(once) () -> Void) -> Void
) {}

// CHECK: public func aliasInFunctionType(_: (consuming OnceFunctions::FnType) -> Swift::Void)
public func aliasInFunctionType(_: (FnType) -> Void) {}

// CHECK: public func aliasConsumingInFunctionType(_: (consuming OnceFunctions::FnType) -> Swift::Void)
public func aliasConsumingInFunctionType(_: (consuming FnType) -> Void) {}

//--- Client.swift

import OnceFunctions

// An explicit `consuming` doesn't change the parameter's ownership.

// CLIENT-DAG: function_ref @$s13OnceFunctions5test1yyyyXEnF
// CLIENT-DAG: function_ref @$s13OnceFunctions17explicitConsumingyyyyXEnF
// CLIENT-DAG: function_ref @$s13OnceFunctions4TestV1fyyyyyXEnXEF
// CLIENT-DAG: function_ref @$s13OnceFunctions31explicitConsumingInFunctionTypeyyyyyXEnXEF

func useOnceFunctions(_ t: borrowing Test) {
  test1 {}
  explicitConsuming {}
  t.f { $0() }
  explicitConsumingInFunctionType { $0() }
  aliasDefault {}
  aliasConsuming {}
  inlinableAliasConsuming {}
  inlinableExplicitConsuming {}
  sendingConsuming {}
  aliasInFunctionType { $0() }
  aliasConsumingInFunctionType { $0() }
}
