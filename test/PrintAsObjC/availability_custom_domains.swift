// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck \
// RUN:   -verify -disable-objc-attr-requires-foundation-module \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-objc-header %S/../Inputs/empty.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -emit-objc-header-path %t/func.h -DFUNC

// RUN: %FileCheck %s --check-prefixes=CHECK-FUNC --input-file %t/func.h
// RUN: %check-in-clang -I %S/../Inputs/custom-modules/availability-domains %t/func.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck \
// RUN:   -verify -disable-objc-attr-requires-foundation-module \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-objc-header %S/../Inputs/empty.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -emit-objc-header-path %t/class.h -DCLASS

// RUN: %FileCheck %s --check-prefixes=CHECK-CLASS --input-file %t/class.h
// RUN: %check-in-clang -I %S/../Inputs/custom-modules/availability-domains %t/class.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck \
// RUN:   -verify -disable-objc-attr-requires-foundation-module \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-objc-header %S/../Inputs/empty.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -emit-objc-header-path %t/class_member.h -DCLASS_MEMBER

// RUN: %FileCheck %s --check-prefixes=CHECK-CLASS-MEMBER --input-file %t/class_member.h
// RUN: %check-in-clang -I %S/../Inputs/custom-modules/availability-domains %t/class_member.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck \
// RUN:   -verify -disable-objc-attr-requires-foundation-module \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-objc-header %S/../Inputs/empty.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -emit-objc-header-path %t/protocol.h -DPROTOCOL

// RUN: %FileCheck %s --check-prefixes=CHECK-PROTOCOL --input-file %t/protocol.h
// RUN: %check-in-clang -I %S/../Inputs/custom-modules/availability-domains %t/protocol.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck \
// RUN:   -verify -disable-objc-attr-requires-foundation-module \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-objc-header %S/../Inputs/empty.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -emit-objc-header-path %t/protocol_member.h -DPROTOCOL_MEMBER

// RUN: %FileCheck %s --check-prefixes=CHECK-PROTOCOL-MEMBER --input-file %t/protocol_member.h
// RUN: %check-in-clang -I %S/../Inputs/custom-modules/availability-domains %t/protocol_member.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck \
// RUN:   -verify -disable-objc-attr-requires-foundation-module \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-objc-header %S/../Inputs/empty.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -emit-objc-header-path %t/extension.h -DEXTENSION

// RUN: %FileCheck %s --check-prefixes=CHECK-EXTENSION --input-file %t/extension.h
// RUN: %check-in-clang -I %S/../Inputs/custom-modules/availability-domains %t/extension.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck \
// RUN:   -verify -disable-objc-attr-requires-foundation-module \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-objc-header %S/../Inputs/empty.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -emit-objc-header-path %t/extension_member.h -DEXTENSION_MEMBER

// RUN: %FileCheck %s --check-prefixes=CHECK-EXTENSION-MEMBER --input-file %t/extension_member.h
// RUN: %check-in-clang -I %S/../Inputs/custom-modules/availability-domains %t/extension_member.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck \
// RUN:   -verify -disable-objc-attr-requires-foundation-module \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-objc-header %S/../Inputs/empty.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -emit-objc-header-path %t/enum.h -DENUM

// RUN: %FileCheck %s --check-prefixes=CHECK-ENUM --input-file %t/enum.h
// RUN: %check-in-clang -I %S/../Inputs/custom-modules/availability-domains %t/enum.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck \
// RUN:   -verify -disable-objc-attr-requires-foundation-module \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-objc-header %S/../Inputs/empty.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -emit-objc-header-path %t/enum_member.h -DENUM_MEMBER

// RUN: %FileCheck %s --check-prefixes=CHECK-ENUM-MEMBER --input-file %t/enum_member.h
// RUN: %check-in-clang -I %S/../Inputs/custom-modules/availability-domains %t/enum_member.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck \
// RUN:   -verify -disable-objc-attr-requires-foundation-module \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-objc-header %S/../Inputs/empty.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -emit-objc-header-path %t/private.h -DPRIVATE

// RUN: %FileCheck %s --check-prefix=CHECK-PRIVATE --input-file %t/private.h
// RUN: %check-in-clang -I %S/../Inputs/custom-modules/availability-domains %t/private.h

// REQUIRES: swift_feature_CustomAvailability
// REQUIRES: objc_interop

import Oceans

#if FUNC

// CHECK-FUNC: SWIFT_EXTERN void arctic_func(void) SWIFT_NOEXCEPT SWIFT_AVAILABILITY_DOMAIN(Arctic,0);
@available(Arctic)
@_cdecl("arctic_func") func arcticFunc() { }

#elseif CLASS

// CHECK-CLASS: SWIFT_AVAILABILITY_DOMAIN(Arctic,0)
// CHECK-CLASS-LABEL: @interface ArcticClass{{$}}
@available(Arctic)
@objc class ArcticClass {
  // CHECK-CLASS-NEXT: - (void)alwaysAvailable;
  @objc func alwaysAvailable() { }
}
// CHECK-CLASS-LABEL: @end

#elseif CLASS_MEMBER

// CHECK-CLASS-MEMBER-LABEL: @interface AvailableClass{{$}}
@objc class AvailableClass {
  // CHECK-CLASS-MEMBER-NEXT: - (void)availableInArctic SWIFT_AVAILABILITY_DOMAIN(Arctic,0);
  @available(Arctic)
  @objc func availableInArctic() { }

  // CHECK-CLASS-MEMBER-NEXT: @property (nonatomic) NSInteger unavailableInArctic SWIFT_AVAILABILITY_DOMAIN(Arctic,1);
  @available(Arctic, unavailable)
  @objc var unavailableInArctic: Int {
    get { 0 }
    set { }
  }
}
// CHECK-CLASS-MEMBER-LABEL: @end

#elseif PROTOCOL

// CHECK-PROTOCOL: SWIFT_AVAILABILITY_DOMAIN(Arctic,0)
// CHECK-PROTOCOL-LABEL: @protocol ArcticProtocol{{$}}
@available(Arctic)
@objc protocol ArcticProtocol {
  // CHECK-PROTOCOL-NEXT: - (void)requirement;
  func requirement()
}
// CHECK-PROTOCOL-LABEL: @end

#elseif PROTOCOL_MEMBER

// CHECK-PROTOCOL-MEMBER-LABEL: @protocol AvailableProtocol{{$}}
@objc protocol AvailableProtocol {
  // CHECK-PROTOCOL-MEMBER-NEXT: - (void)availableInArctic SWIFT_AVAILABILITY_DOMAIN(Arctic,0);
  @available(Arctic)
  func availableInArctic()

  func requirement()
}
// CHECK-PROTOCOL-MEMBER-LABEL: @end

#elseif EXTENSION

// CHECK-EXTENSION-LABEL: @interface AvailableClass{{$}}
@objc class AvailableClass {
}
// CHECK-EXTENSION-LABEL: @end

// CHECK-EXTENSION-LABEL: SWIFT_AVAILABILITY_DOMAIN(Arctic,0)
// CHECK-EXTENSION-NEXT: @interface AvailableClass (SWIFT_EXTENSION(availability_custom_domains))
@available(Arctic)
extension AvailableClass {
  // CHECK-EXTENSION-NEXT: - (void)alwaysAvailable;
  @objc func alwaysAvailable() { }
}
// CHECK-EXTENSION-LABEL: @end

#elseif EXTENSION_MEMBER

// CHECK-EXTENSION-MEMBER-LABEL: @interface AvailableClass{{$}}
@objc class AvailableClass {
}
// CHECK-EXTENSION-MEMBER-LABEL: @end

// CHECK-EXTENSION-MEMBER-LABEL: @interface AvailableClass (SWIFT_EXTENSION(availability_custom_domains))
extension AvailableClass {
  // CHECK-EXTENSION-MEMBER-NEXT: - (void)availableInArctic SWIFT_AVAILABILITY_DOMAIN(Arctic,0);
  @available(Arctic)
  @objc func availableInArctic() { }
}
// CHECK-EXTENSION-MEMBER-LABEL: @end

#elseif ENUM

// FIXME: [availability] Availability attribute is missing
// CHECK-ENUM-LABEL: typedef SWIFT_ENUM(NSInteger, ArcticEnum, closed)
@available(Arctic)
@objc enum ArcticEnum: Int {
  // CHECK-ENUM-NEXT: ArcticEnumAvailable = 0,
  case available
}
// CHECK-ENUM-NEXT: };

#elseif ENUM_MEMBER

// FIXME: [availability] Availability attribute is missing
// CHECK-ENUM-MEMBER-LABEL: typedef SWIFT_ENUM(NSInteger, AvailableEnum, closed)
@objc enum AvailableEnum: Int {
  // CHECK-ENUM-MEMBER-NEXT: AvailableEnumAvailableInArctic = 0,
  @available(Arctic)
  case availableInArctic
}
// CHECK-ENUM-MEMBER-NEXT: };

#elseif PRIVATE

// CHECK-PRIVATE-NOT: @import Oceans;
// CHECK-PRIVATE-NOT: @interface PrivateClass
@available(Arctic)
@objc private class PrivateClass { }

// CHECK-PRIVATE-LABEL: @interface ClassWithPrivateMember{{$}}
@objc class ClassWithPrivateMember {
  // CHECK-PRIVATE-NOT: - (void)member;
  @available(Arctic)
  @objc private func member() { }
}

#endif
