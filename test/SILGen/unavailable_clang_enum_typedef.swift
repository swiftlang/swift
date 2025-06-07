// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-emit-silgen %t/Overlay.swift -I %t -import-objc-header %t/MyOptions.h -unavailable-decl-optimization=none | %FileCheck %t/Overlay.swift
// RUN: %target-swift-emit-silgen %t/Overlay.swift -I %t -import-objc-header %t/MyOptions.h -unavailable-decl-optimization=stub | %FileCheck %t/Overlay.swift
// RUN: %target-swift-emit-silgen %t/Overlay.swift -I %t -import-objc-header %t/MyOptions.h -unavailable-decl-optimization=complete | %FileCheck %t/Overlay.swift

//--- MyOptions.h

__attribute__((availability(swift, unavailable, message="Unavailable in Swift")))
typedef enum : int {
  SomeOption,
} MyOptions;

typedef MyOptions MyOptionsTypedef;

//--- Overlay.swift

// This really shouldn't be allowed, but it is.
let _ = MyOptionsTypedef(rawValue: 1)

// CHECK-LABEL: sil shared [transparent] [serialized]{{.*}} @$sSo9MyOptionsa8rawValueABs5Int32V_tcfC : $@convention(method) (Int32, @thin MyOptions.Type) -> MyOptions {
// CHECK-NOT:     _diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$sSo9MyOptionsa8rawValueABs5Int32V_tcfC'
