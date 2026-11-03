// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-emit-silgen -enable-experimental-feature ObjCDirect \
// RUN:   %t/swift.swift | %FileCheck %s
// RUN: %target-swift-emit-silgen -enable-experimental-feature ObjCDirect \
// RUN:   -import-objc-header %t/Codec.h %t/imported.swift \
// RUN:   | %FileCheck %s --check-prefix=IMPORTED
// RUN: %target-swift-emit-silgen -import-objc-header %t/Codec.h \
// RUN:   %t/imported.swift | %FileCheck %s --check-prefix=NOFEATURE

//--- swift.swift

import Foundation

final class DelegatingInit: NSObject {
  var v: Int
  @objcDirect init(value: Int) { v = value; super.init() }
  override convenience init() { self.init(value: 7) }
  @objcDirect convenience init(twice: Int) { self.init(value: twice * 2) }
}

// CHECK-LABEL: sil {{.*}}DelegatingInitCACycfc :
// CHECK-NOT: objc_method
// CHECK: function_ref @{{.*}}DelegatingInitC5valueACSi_tcfc
// CHECK-NOT: objc_method
// CHECK: } // end sil function

// CHECK-LABEL: sil {{.*}}DelegatingInitC5twiceACSi_tcfc :
// CHECK-NOT: objc_method
// CHECK: function_ref @{{.*}}DelegatingInitC5valueACSi_tcfc
// CHECK-NOT: objc_method
// CHECK: } // end sil function

// In a non-final class, delegation to another direct initializer is static,
// and to a designated one it dispatches, so a subclass's override runs.
class OpenDelegating: NSObject {
  var v: Int
  init(value: Int) { v = value; super.init() }
  @objcDirect convenience init(twice: Int) { self.init(value: twice * 2) }
  @objcDirect convenience init(quad: Int) { self.init(twice: quad * 2) }
}

// CHECK-LABEL: sil {{.*}}OpenDelegatingC5twiceACSi_tcfc :
// CHECK: class_method {{.*}}, #OpenDelegating.init!allocator
// CHECK: } // end sil function

// CHECK-LABEL: sil {{.*}}OpenDelegatingC4quadACSi_tcfc :
// CHECK-NOT: objc_method
// CHECK: function_ref @{{.*}}OpenDelegatingC5twiceACSi_tcfc
// CHECK-NOT: objc_method
// CHECK: } // end sil function

//--- Codec.h

#import <Foundation/Foundation.h>

// With no designated initializers marked, this one is designated.
@interface OCodec : NSObject
- (instancetype)initWithSeparator:(NSString *)separator
    __attribute__((objc_direct));
@end

@interface OConv : NSObject
- (instancetype)init NS_DESIGNATED_INITIALIZER;
- (instancetype)initWithSeparator:(NSString *)separator
    __attribute__((objc_direct));
@end

// Each gets a convenience copy of its superclass's initWithSeparator:.
@interface OCodecKid : OCodec
- (instancetype)initWithTag:(NSInteger)tag NS_DESIGNATED_INITIALIZER;
@end

@interface OConvKid : OConv
- (instancetype)initWithTag:(NSInteger)tag NS_DESIGNATED_INITIALIZER;
@end

//--- imported.swift

import Foundation

extension OCodec {
  convenience init(tab: ()) { self.init(separator: "\t") }
}

extension OConv {
  convenience init(tab: ()) { self.init(separator: "\t") }
}

extension OCodecKid {
  convenience init(kidTab: ()) { self.init(separator: "\t") }
}

extension OConvKid {
  convenience init(kidTab: ()) { self.init(separator: "\t") }
}

// A designated one keeps the message send.
// IMPORTED-LABEL: // OCodec.init(tab:)
// IMPORTED: objc_method {{.*}} #OCodec.init!initializer.foreign
// IMPORTED: } // end sil function

// A convenience one gets a static reference to the foreign entry point.
// IMPORTED-LABEL: // OConv.init(tab:)
// IMPORTED-NOT: = objc_method
// IMPORTED: function_ref @$sSo5OConvC9separator{{.*}}tcfcTo : $@convention(objc_method)
// IMPORTED-NOT: = objc_method
// IMPORTED: } // end sil function

// The copy of a designated one keeps the message send.
// IMPORTED-LABEL: // OCodecKid.init(kidTab:)
// IMPORTED: objc_method {{.*}} #OCodecKid.init!initializer.foreign
// IMPORTED: } // end sil function

// The copy of a convenience one is static, like the original.
// IMPORTED-LABEL: // OConvKid.init(kidTab:)
// IMPORTED-NOT: = objc_method
// IMPORTED: function_ref @$sSo8OConvKidC9separator{{.*}}tcfcTo : $@convention(objc_method)
// IMPORTED-NOT: = objc_method
// IMPORTED: } // end sil function

// NOFEATURE-LABEL: // OCodec.init(tab:)
// NOFEATURE: objc_method {{.*}} #OCodec.init!initializer.foreign
// NOFEATURE: } // end sil function
// NOFEATURE-LABEL: // OConv.init(tab:)
// NOFEATURE: objc_method {{.*}} #OConv.init!initializer.foreign
// NOFEATURE: } // end sil function
// NOFEATURE-LABEL: // OCodecKid.init(kidTab:)
// NOFEATURE: objc_method {{.*}} #OCodecKid.init!initializer.foreign
// NOFEATURE: } // end sil function
// NOFEATURE-LABEL: // OConvKid.init(kidTab:)
// NOFEATURE: objc_method {{.*}} #OConvKid.init!initializer.foreign
// NOFEATURE: } // end sil function
