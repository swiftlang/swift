// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect
// REQUIRES: swift_feature_ObjCImplementation

// Storage with an objc_direct accessor is imported 'final', so it and its
// accessors must be 'public', not 'open'; the AST verifier checks this.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -import-objc-header %t/Box.h %t/use.swift
// RUN: %target-swift-frontend -typecheck -import-objc-header %t/Box.h \
// RUN:   -enable-experimental-feature ObjCDirect %t/use.swift
// RUN: %target-swift-frontend -typecheck -import-objc-header %t/Box.h \
// RUN:   -enable-experimental-feature ObjCDirect \
// RUN:   -enable-experimental-feature ObjCImplementation %t/impl.swift

//--- Box.h

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN
@interface Box : NSObject
- (nullable NSString *)objectForKeyedSubscript:(NSString *)key
    __attribute__((objc_direct));
- (void)setObject:(nullable NSString *)obj forKeyedSubscript:(NSString *)key
    __attribute__((objc_direct));
@end

@interface Gauge : NSObject
@property NSInteger level;
- (NSInteger)level __attribute__((objc_direct));
@property NSInteger limit;
- (void)setLimit:(NSInteger)limit __attribute__((objc_direct));
- (nullable NSString *)objectAtIndexedSubscript:(NSUInteger)i;
- (void)setObject:(nullable NSString *)obj atIndexedSubscript:(NSUInteger)i
    __attribute__((objc_direct));
@end
NS_ASSUME_NONNULL_END

//--- use.swift

import Foundation

func use(_ b: Box, _ g: Gauge) -> String? {
  b["k"] = "v"
  g.level = g.limit
  g.limit = g.level
  g[0] = g[1]
  return b["k"]
}

//--- impl.swift

import Foundation

@objc @implementation extension Box {
  @objcDirect subscript(key: String) -> String? {
    get { nil }
    set { }
  }
}
