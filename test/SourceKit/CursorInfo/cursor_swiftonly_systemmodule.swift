// Check whether we correctly specify `someFunc` as part of a system module.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/EmptySDK)
// RUN: split-file %s %t

// RUN: mkdir -p %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule
// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name SomeModule \
// RUN:     -o %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -swift-version 5 \
// RUN:     %t/SomeModule.swift

// Doesn't matter whether the framework is system or not, SomeModule is part
// of the mock SDK.
// RUN: %sourcekitd-test -req=cursor -pos=4:3 %t/main.swift -- \
// RUN:     -sdk %t/SDK \
// RUN:     -F %t/SDK/Frameworks \
// RUN:     -target %target-triple \
// RUN:     %t/main.swift \
// RUN: | %FileCheck %s

// No SDK, fallback to checking whether the search path is system or not
// RUN: %sourcekitd-test -req=cursor -pos=4:3 %t/main.swift -- \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -target %target-triple \
// RUN:     %t/main.swift \
// RUN: | %FileCheck %s

// Empty SDK, so shouldn't be a system module any more.
// RUN: %sourcekitd-test -req=cursor -pos=4:3 %t/main.swift -- \
// RUN:     -sdk %t/EmptySDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -target %target-triple \
// RUN:     %t/main.swift \
// RUN: | %FileCheck %s --check-prefix=NOSYS

// CHECK: source.lang.swift.ref.function.free ()
// CHECK-NEXT: someFunc()
// CHECK-NEXT: s:10SomeModule8someFuncyyF
// CHECK-NEXT: source.lang.swift
// CHECK-NEXT: () -> ()
// CHECK-NEXT: $syycD
// CHECK-NEXT: SomeModule
// CHECK-NEXT: SYSTEM
// CHECK-NEXT: <Declaration>func someFunc()</Declaration>
// CHECK-NEXT: <decl.function.free><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>someFunc</decl.name>()</decl.function.free>

// NOSYS: someFunc()
// NOSYS: SomeModule
// NOSYS-NOT: SYSTEM

//--- SomeModule.swift
/// Doc comment for 'someFunc()'
public func someFunc() {}

//--- main.swift
import SomeModule

func test() {
  someFunc()
}
