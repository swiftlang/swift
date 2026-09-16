// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module %s \
// RUN:   -module-name A -swift-version 5 \
// RUN:   -target %target-swift-6.1-abi-triple \
// RUN:   -enable-library-evolution \
// RUN:     -emit-module-path %t/A.swiftmodule \
// RUN:     -emit-module-interface-path %t/A.swiftinterface

// RUN: %FileCheck %s --check-prefixes=CHECK,NON-ISOLATED-DEFAULT < %t/A.swiftinterface
// RUN: %FileCheck %s --check-prefix=NO-FEATURE-GUARD < %t/A.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/A.swiftinterface)

// RUN: %target-swift-frontend -emit-module %s \
// RUN:   -module-name B -swift-version 5 \
// RUN:   -target %target-swift-6.1-abi-triple \
// RUN:   -enable-library-evolution \
// RUN:   -default-isolation MainActor \
// RUN:     -emit-module-path %t/B.swiftmodule \
// RUN:     -emit-module-interface-path %t/B.swiftinterface

// RUN: %FileCheck %s --check-prefixes=CHECK,MAIN-ACTOR-DEFAULT < %t/B.swiftinterface
// RUN: %FileCheck %s --check-prefix=NO-FEATURE-GUARD < %t/B.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/B.swiftinterface)

// REQUIRES: concurrency

// NO-FEATURE-GUARD-NOT: {{^#}}

// CHECK-LABEL: @_Concurrency::MainActor public protocol MainActorProto {
@MainActor public protocol MainActorProto {
  func req()
}

// CHECK-LABEL: public actor A1 {
// CHECK: {{^ (@objc )?deinit$}}
public actor A1 {
  deinit {}
}

// CHECK-LABEL: public actor A2 {
// CHECK: {{^ (@objc )?@_Concurrency::MainActor deinit$}}
public actor A2 {
  @MainActor deinit {}
}

// CHECK-LABEL: public actor A3 {
// CHECK: {{^ (@objc )?isolated deinit$}}
public actor A3 {
  isolated deinit {}
}

// CHECK-LABEL: public actor A4 {
// CHECK: {{^ (@objc )?nonisolated deinit$}}
public actor A4 {
  nonisolated deinit {}
}

// CHECK-LABEL: public actor A5 : {{([AB]::)?}}MainActorProto {
// CHECK: @_Concurrency::MainActor public func req()
// CHECK: {{^ (@objc )?deinit$}}
public actor A5: MainActorProto {
  @MainActor public func req() { }
}

// CHECK-LABEL: open class C0 {
// CHECK: {{^ (@objc )?deinit$}}
open class C0 {
  deinit {}
}

// CHECK-LABEL: public class C1 {
// CHECK: {{^ (@objc )?deinit$}}
public class C1 {
  deinit {}
}

// CHECK-LABEL: open class C2 {
// CHECK: {{^ (@objc )?@_Concurrency::MainActor deinit$}}
open class C2 {
  @MainActor deinit {}
}

// CHECK-LABEL: public class C3 {
// CHECK: {{^ (@objc )?@_Concurrency::MainActor deinit$}}
public class C3 {
  @MainActor deinit {}
}

// CHECK-LABEL: open class C4 {
// CHECK: {{^ (@objc )?isolated deinit$}}
@MainActor
open class C4 {
  isolated deinit {}
}

// CHECK-LABEL: public class C5 {
// CHECK: {{^ (@objc )?isolated deinit$}}
@MainActor
public class C5 {
  isolated deinit {}
}

// CHECK-LABEL: open class C6 {
// CHECK: {{^ (@objc )?nonisolated deinit$}}
@MainActor
open class C6 {
  nonisolated deinit {}
}

// CHECK-LABEL: public class C7 {
// CHECK: {{^ (@objc )?nonisolated deinit$}}
@MainActor
public class C7 {
  nonisolated deinit {}
}

// CHECK-LABEL: public class C8 {
// CHECK: {{^ (@objc )?isolated deinit$}}
@MainActor
@preconcurrency
public class C8 {
  isolated deinit {}
}

// CHECK-LABEL: public class C9 {
// CHECK: {{^ (@objc )?@_Concurrency::MainActor deinit$}}
@preconcurrency
public class C9 {
  @MainActor deinit {}
}

// CHECK-LABEL: public class C10 : {{([AB]::)?}}MainActorProto {
// CHECK: @_Concurrency::MainActor public func req()
// NON-ISOLATED-DEFAULT: {{^ (@objc )?deinit$}}
// MAIN-ACTOR-DEFAULT: {{^ (@objc )?@_Concurrency::MainActor deinit$}}
public class C10: MainActorProto {
  public func req() { }
}

// CHECK-LABEL: open class Base1 {
// CHECK: {{^ (@objc )?isolated deinit$}}
@MainActor
open class Base1 {
  public init() {}
  isolated deinit {}
}

// CHECK-LABEL: open class Derived1 : {{([AB]::)?}}Base1 {
// CHECK: {{^ (@objc )?@_Concurrency::MainActor deinit$}}
open class Derived1: Base1 {}

// CHECK-LABEL: open class Derived1a : {{([AB]::)?}}Derived1 {
// CHECK: {{^ (@objc )?@_Concurrency::MainActor deinit$}}
open class Derived1a: Derived1 {}

// CHECK-LABEL: open class Derived1b : {{([AB]::)?}}Base1 {
// CHECK: {{^ (@objc )?isolated deinit$}}
open class Derived1b: Base1 {
  isolated deinit {}
}

// CHECK-LABEL: open class Base2 {
// CHECK: {{^ (@objc )?@_Concurrency::MainActor deinit$}}
open class Base2 {
  public init() {}
  @MainActor deinit {}
}

// CHECK-LABEL: open class Derived2 : {{([AB]::)?}}Base2 {
// CHECK: {{^ (@objc )?@_Concurrency::MainActor deinit$}}
open class Derived2: Base2 {}

// CHECK-LABEL: open class Base3 {
// CHECK: {{^ (@objc )?deinit$}}
@MainActor
open class Base3 {
  public init() {}
  deinit {}
}

// CHECK-LABEL: open class Derived3 : {{([AB]::)?}}Base3 {
// CHECK: {{^ (@objc )?deinit$}}
open class Derived3: Base3 {}

// CHECK-LABEL: public actor CustomActor {
// CHECK: {{^ (@objc )?deinit$}}
@globalActor
public actor CustomActor {
  public static let shared = CustomActor()
}

// CHECK-LABEL: open class Base4 {
// CHECK: {{^ (@objc )?isolated deinit$}}
@CustomActor
open class Base4 {
  public init() {}
  isolated deinit {}
}

// CHECK-LABEL: open class Derived4 : {{([AB]::)?}}Base4 {
// CHECK: {{^ (@objc )?@([AB]::)?CustomActor deinit$}}
open class Derived4: Base4 {}
