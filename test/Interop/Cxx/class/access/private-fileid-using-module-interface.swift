// RUN: %target-swift-ide-test -print-module -module-to-print=PrivateFileIDUsing -print-access -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s --check-prefixes=CHECK,BLESSED
// RUN: %target-swift-ide-test -print-module -module-to-print=PrivateFileIDUsing -print-access -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default -Xcc -DBLESSED= | %FileCheck %s --check-prefixes=CHECK,CURSED

// CHECK:      public struct PublUser {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public var protField: CInt
// BLESSED-NEXT: private var protFieldNoUsing: CInt
// BLESSED-NEXT: private var protFieldProtUsing: CInt
// CHECK-NEXT:   public func protMethod()
// CHECK-NEXT:   public var publField: CInt
// CHECK-NEXT:   public func publMethod()
// CHECK-NEXT: }

// CHECK:      public struct PrivUser {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public var protField: CInt
// BLESSED-NEXT: private var protFieldNoUsing: CInt
// BLESSED-NEXT: private var protFieldProtUsing: CInt
// CHECK-NEXT:   public func protMethod()
// CHECK-NEXT:   public var publField: CInt
// CHECK-NEXT:   public func publMethod()
// CHECK-NEXT: }

// CHECK:      public struct PublPrivUser {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public var protField: CInt
// BLESSED-NEXT: @available(*, unavailable, message: "this base member is not accessible because of private inheritance")
// BLESSED-NEXT: private var protFieldNoUsing: CInt
// BLESSED-NEXT: private var protFieldProtUsing: CInt
// CHECK-NEXT:   public func protMethod()
// CHECK-NEXT:   public var publField: CInt
// CHECK-NEXT:   public func publMethod()
// CHECK-NEXT: }
