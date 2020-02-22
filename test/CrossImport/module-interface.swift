// This file tests that we emit cross-imports into module interfaces.

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/lib-templates/* %t/

//
// Should pass with -enable-cross-import-overlays
//

// RUN: %target-swift-frontend -enable-cross-import-overlays -I %t/lib/swift -typecheck -emit-module-interface-path %t.swiftinterface %s -module-name ClientLibrary -swift-version 5 -enable-library-evolution
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %FileCheck -check-prefix NEGATIVE %s < %t.swiftinterface

//
// Should fail with -disable-cross-import-overlays
//

// RUN: not %target-swift-frontend -disable-cross-import-overlays -I %t/lib/swift -typecheck -emit-module-interface-path %t.swiftinterface %s -module-name ClientLibrary -swift-version 5 -enable-library-evolution 2>/dev/null

//
// Should fail by default
//

// RUN: not %target-swift-frontend -I %t/lib/swift -typecheck -emit-module-interface-path %t.swiftinterface %s -module-name ClientLibrary -swift-version 5 -enable-library-evolution 2>/dev/null


import DeclaringLibrary
import BystandingLibrary

public func fn(_: DeclaringLibraryTy, _: BystandingLibraryTy, _: OverlayLibraryTy) {}

public func alias(_: DeclaringLibrary.OverlayLibraryTy) {}

public func shadow(_: DeclaringLibrary.ShadowTy, _: ShadowTy) {}

// CHECK: // swift-interface-format-version
// CHECK: // swift-module-flags: {{.*}} -module-name ClientLibrary

// CHECK-DAG: import Swift
// CHECK-DAG: import BystandingLibrary
// CHECK-DAG: import _OverlayLibrary
// NEGATIVE-NOT: import DeclaringLibrary

// CHECK-DAG: public func fn(_: DeclaringLibrary.DeclaringLibraryTy, _: BystandingLibrary.BystandingLibraryTy, _: _OverlayLibrary.OverlayLibraryTy)
// CHECK-DAG: public func alias(_: _OverlayLibrary.OverlayLibraryTy)
// CHECK-DAG: public func shadow(_: _OverlayLibrary.ShadowTy, _: _OverlayLibrary.ShadowTy)
