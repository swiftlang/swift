// Please keep this file in alphabetical order!

// REQUIRES: objc_interop

// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift -disable-objc-attr-requires-foundation-module
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -import-objc-header %S/Inputs/circularity.h -emit-module -o %t %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -import-objc-header %S/Inputs/circularity.h -parse-as-library %t/circularity.swiftmodule -typecheck -verify -emit-objc-header-path %t/circularity.h

// RUN: %FileCheck %s < %t/circularity.h

// RUN: %check-in-clang %t/circularity.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments %t/circularity.h

import Foundation

// CHECK-LABEL: @interface A1 : ProtoImpl
class A1: ProtoImpl {
  // CHECK: // 'test(_:)' below
  @objc func test(_: NeedsProto<A2>) {}
} // CHECK: @end
// CHECK-LABEL: @interface A2 : ProtoImpl
class A2: ProtoImpl {
  // CHECK: - (void)test:
  @objc func test(_: NeedsProto<A1>) {}
} // CHECK: @end

// CHECK-LABEL: @interface B1 : ProtoImpl
class B1: ProtoImpl {
  // CHECK: // 'test(_:)' below
  @objc func test(_: NeedsProto<B2>) {}
} // CHECK: @end
// CHECK-LABEL: @interface B2 : ProtoImpl
class B2: ProtoImpl {
} // CHECK: @end

// CHECK-LABEL: @interface C1 : ProtoImpl
class C1: ProtoImpl {
  // CHECK: // 'test(_:)' below
  @objc func test(_: NeedsProto<C2>) {}
} // CHECK: @end
// CHECK-LABEL: @protocol C2 <Proto>
@objc protocol C2: Proto {
} // CHECK: @end

// CHECK-LABEL: @interface D1 : ProtoImpl
class D1: ProtoImpl {
  // CHECK: // 'test(_:)' below
  @objc func test(_: NeedsProto<D2>) {}
} // CHECK: @end
// CHECK-LABEL: @protocol D2 <Proto>
@objc protocol D2: Proto {
  // CHECK: - (void)test:
  func test(_: NeedsProto<D1>)
} // CHECK: @end

// CHECK-LABEL: @interface D4 : ProtoImpl
// CHECK: // 'test(_:)' below
// CHECK: @end
// CHECK-LABEL: @protocol D3 <Proto>
@objc protocol D3: Proto {
  // CHECK: - (void)test:
  func test(_: NeedsProto<D4>)
} // CHECK: @end
// Moved ahead.
class D4: ProtoImpl {
  @objc func test(_: NeedsProto<D3>) {}
}

// CHECK-LABEL: @interface E2 : ProtoImpl
// CHECK: @end
// CHECK-LABEL: @protocol E1 <Proto>
@objc protocol E1: Proto {
  // CHECK: - (void)test:
  func test(_: NeedsProto<E2>)
} // CHECK: @end
// Moved ahead.
class E2: ProtoImpl {}

// CHECK-LABEL: @interface F1 : ProtoImpl
class F1: ProtoImpl {
} // CHECK: @end
// CHECK-LABEL: @interface F2 : ProtoImpl
// CHECK: @end
// CHECK-LABEL: @interface F1 (SWIFT_EXTENSION(circularity))
extension F1 {
  // CHECK: - (void)test:
  @objc func test(_: NeedsProto<F2>) {}
} // CHECK: @end
// Moved ahead.
class F2: ProtoImpl {}

// CHECK-LABEL: @interface G1 : ProtoImpl
class G1: ProtoImpl {
} // CHECK: @end
// CHECK-LABEL: @protocol G2 <Proto>
// CHECK: @end
// CHECK-LABEL: @interface G1 (SWIFT_EXTENSION(circularity))
extension G1 {
  // CHECK: - (void)test:
  @objc func test(_: NeedsProto<G2>) {}
} // CHECK: @end
// Moved ahead.
@objc protocol G2: Proto {}

// CHECK-LABEL: @interface H1 : ProtoImpl
class H1: ProtoImpl {
  // CHECK: 'test(_:)' below
  @objc func test(_: NeedsProto<H2>) {}
  // CHECK: 'anotherTest(_:)' below
  @objc func anotherTest(_: NeedsProto<H3>) {}
} // CHECK: @end
// CHECK-LABEL: @interface H2 : ProtoImpl
class H2: ProtoImpl {
  // CHECK: // 'test(_:)' below
  @objc func test(_: NeedsProto<H3>) {}
  // CHECK: - (void)anotherTest:
  @objc func anotherTest(_: NeedsProto<H1>) {}
} // CHECK: @end
// CHECK-LABEL: @interface H3 : ProtoImpl
class H3: ProtoImpl {
  // CHECK: - (void)test:
  @objc func test(_: NeedsProto<H1>) {}
  // CHECK: - (void)anotherTest:
  @objc func anotherTest(_: NeedsProto<H2>) {}
} // CHECK: @end

// CHECK-LABEL: @interface I1 : Parent
class I1 : Parent {
  // CHECK: // 'test(_:)' below
  @objc func test(_: NeedsParent<I2>) {}
} // CHECK: @end
// CHECK-LABEL: @interface I2 : Parent
class I2 : Parent {
  // CHECK: - (void)test:
  @objc func test(_: NeedsParent<I1>) {}
} // CHECK: @end

// CHECK-LABEL: @interface J1 : Parent
class J1 : Parent {
  // CHECK: - (void)test:
  @objc func test(_: Unconstrained<J2>) {}
} // CHECK: @end
// CHECK-LABEL: @interface J2 : Parent
class J2 : Parent {
  // CHECK: - (void)test:
  @objc func test(_: Unconstrained<J1>) {}
} // CHECK: @end

// CHECK-LABEL: @protocol K1 <Proto>
@objc protocol K1 : Proto {
  // CHECK: - (void)test:
  @objc func test(_: Unconstrained<K2>)
} // CHECK: @end
// CHECK-LABEL: @protocol K2 <Proto>
@objc protocol K2 : Proto {
  // CHECK: - (void)test:
  @objc func test(_: Unconstrained<K1>)
} // CHECK: @end


// CHECK-LABEL: @interface NA2 : ProtoImpl
// CHECK: @end
// CHECK-LABEL: @protocol NA1
// CHECK: - (void)test:
// CHECK: @end
// CHECK-LABEL: @interface NA3 : NSObject <NA1>
// CHECK: @end
@objc protocol NA1 {
  @objc optional func test(_: NeedsProto<NA2>)
}
class NA2: ProtoImpl {}
class NA3: NSObject, NA1 {}

// CHECK-LABEL: @interface NB1 : ProtoImpl
// CHECK: @end
// CHECK-LABEL: @protocol NB2
// CHECK: - (void)test:
// CHECK: @end
// CHECK-LABEL: @interface NB3 : NSObject <NB2>
// CHECK: @end
class NB1: ProtoImpl {}
@objc protocol NB2 {
  @objc optional func test(_: NeedsProto<NB1>)
}
class NB3: NSObject, NB2 {}

// CHECK-LABEL: @interface NC2 : ProtoImpl
// CHECK: @end
// CHECK-LABEL: @protocol NC3
// CHECK: - (void)test:
// CHECK: @end
// CHECK-LABEL: @interface NC1 : NSObject <NC3>
// CHECK: @end
class NC1: NSObject, NC3 {}
class NC2: ProtoImpl {}
@objc protocol NC3 {
  @objc optional func test(_: NeedsProto<NC2>)
}

// CHECK-LABEL: @interface ND3 : ProtoImpl
// CHECK: @end
// CHECK-LABEL: @protocol ND2
// CHECK: - (void)test:
// CHECK: @end
// CHECK-LABEL: @interface ND1 : NSObject <ND2>
// CHECK: @end
class ND1: NSObject, ND2 {}
@objc protocol ND2 {
  @objc optional func test(_: NeedsProto<ND3>)
}
class ND3: ProtoImpl {}

// CHECK-LABEL: @interface NE3 : ProtoImpl
// CHECK: @end
// CHECK-LABEL: @protocol NE1
// CHECK: - (void)test:
// CHECK: @end
// CHECK-LABEL: @interface NE2 : NSObject <NE1>
// CHECK: @end
@objc protocol NE1 {
  @objc optional func test(_: NeedsProto<NE3>)
}
class NE2: NSObject, NE1 {}
class NE3: ProtoImpl {}

// CHECK-LABEL: @interface NF2 : ProtoImpl
// CHECK: @end
// CHECK-LABEL: @protocol NF1
// CHECK: - (void)test:
// CHECK: @end
// CHECK-LABEL: @interface NF3 : NSObject <NF1>
// CHECK: @end
@objc protocol NF1 {
  @objc optional func test(_: NeedsProto<NF2>)
}
class NF2: ProtoImpl {}
class NF3: NSObject, NF1 {}

// CHECK-LABEL: @interface ZZZ_EOF : NSObject
class ZZZ_EOF : NSObject {
} // CHECK: @end

// CHECK-LABEL: @interface A1 (SWIFT_EXTENSION(circularity))
// CHECK: - (void)test:
// CHECK: @end

// CHECK-LABEL: @interface B1 (SWIFT_EXTENSION(circularity))
// CHECK: - (void)test:
// CHECK: @end

// CHECK-LABEL: @interface C1 (SWIFT_EXTENSION(circularity))
// CHECK: - (void)test:
// CHECK: @end

// CHECK-LABEL: @interface D1 (SWIFT_EXTENSION(circularity))
// CHECK: - (void)test:
// CHECK: @end

// CHECK-LABEL: @interface D4 (SWIFT_EXTENSION(circularity))
// CHECK: - (void)test:
// CHECK: @end

// CHECK-LABEL: @interface H1 (SWIFT_EXTENSION(circularity))
// CHECK: - (void)test:
// CHECK-NOT: @end
// CHECK: - (void)anotherTest:
// CHECK: @end

// CHECK-LABEL: @interface H2 (SWIFT_EXTENSION(circularity))
// CHECK: - (void)test:
// CHECK: @end

// CHECK-LABEL: @interface I1 (SWIFT_EXTENSION(circularity))
// CHECK: - (void)test:
// CHECK: @end

