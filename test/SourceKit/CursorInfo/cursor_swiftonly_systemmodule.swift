// BEGIN SomeModule.swift

/// Doc comment for 'someFunc()'
public func someFunc() {}

// BEGIN main.swift
import SomeModule

func test() {
  someFunc()
}

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: mkdir -p %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule
// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name SomeModule \
// RUN:     -o %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -swift-version 5 \
// RUN:     %t/SomeModule.swift

// RUN: %sourcekitd-test -req=cursor -pos=4:3 %t/main.swift -- \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -target %target-triple \
// RUN:     %t/main.swift \
// RUN: | %FileCheck %s

// CHECK: source.lang.swift.ref.function.free ()
// CHECK-NEXT: someFunc()
// CHECK-NEXT: s:10SomeModule8someFuncyyF
// CHECK-NEXT: () -> ()
// CHECK-NEXT: $syycD
// CHECK-NEXT: SomeModule
// CHECK-NEXT: SYSTEM
// CHECK-NEXT: <Declaration>func someFunc()</Declaration>
// CHECK-NEXT: <decl.function.free><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>someFunc</decl.name>()</decl.function.free>
// CHECK-NEXT: </LocalizationKey>
