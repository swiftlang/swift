// Check that a failed type lookup reports types and protocols that more than
// one image defines.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift -parse-as-library -c %t/Core.swift -module-name Core -emit-module -emit-module-path %t/Core.swiftmodule -o %t/Core.o
// RUN: %target-build-swift-dylib(%t/%target-library-name(X)) %t/X.swift %t/Core.o -I %t -module-name X -emit-module -emit-module-path %t/X.swiftmodule
// RUN: %target-build-swift-dylib(%t/%target-library-name(Y)) %t/Y.swift %t/Core.o -I %t -module-name Y -emit-module -emit-module-path %t/Y.swiftmodule
// RUN: %target-build-swift %t/main.swift -I %t -L %t -lX -lY -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main %t/%target-library-name(X) %t/%target-library-name(Y)
// RUN: %target-not-crash %target-run %t/main %t/%target-library-name(X) %t/%target-library-name(Y) 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// Each dylib gets its own copy of Core only with two-level namespaces.
// REQUIRES: VENDOR=apple
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

//--- Core.swift
public protocol P {}
public struct Wrapper<T> {
  public struct Dup {}
}

//--- X.swift
import Core
import Darwin

public struct G<T: P, U, V> {}

public let xTimespec: Any.Type = timespec.self

//--- Y.swift
import Core
import Darwin

public struct S: P {}

public let yTimespec: Any.Type = timespec.self

//--- main.swift
import Core
import Darwin
import X
import Y

_ = (xTimespec, yTimespec)

// CHECK: failed type lookup for X.G<Y.S, Core.Wrapper<Swift.Int>.Dup, __C.timespec>: subject type {{.*}} does not conform to protocol P
// CHECK-NEXT: 2 copies of Core.P are loaded, from: {{.*}}lib{{[XY]}}.dylib, {{.*}}lib{{[XY]}}.dylib. This may cause protocol conformance checks to fail, type lookups to fail, runtime misbehavior, and crashes.
// CHECK: 2 copies of Core.Wrapper.Dup are loaded, from: {{.*}}lib{{[XY]}}.dylib, {{.*}}lib{{[XY]}}.dylib. This may cause protocol conformance checks to fail, type lookups to fail, runtime misbehavior, and crashes.
// CHECK: 2 copies of Core.Wrapper are loaded, from: {{.*}}lib{{[XY]}}.dylib, {{.*}}lib{{[XY]}}.dylib. This may cause protocol conformance checks to fail, type lookups to fail, runtime misbehavior, and crashes.
// CHECK-NOT: copies of __C.timespec
// CHECK: failed type lookup for X.G<Y.S, Core.Wrapper<Swift.Int>.Dup, __C.timespec>.Type
print(G<S, Wrapper<Int>.Dup, timespec>.self)
