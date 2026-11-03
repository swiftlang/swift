// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify \
// RUN:   -enable-experimental-feature ObjCDirect \
// RUN:   -import-objc-header %t/Gauge.h %t/main.swift
// RUN: %target-swift-frontend -emit-module -module-name Lib -o %t \
// RUN:   -enable-experimental-feature ObjCDirect %t/Lib.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t \
// RUN:   -import-objc-header %t/Gauge.h %t/client.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t \
// RUN:   -import-objc-header %t/Gauge.h %t/client.swift \
// RUN:   -Wwarning ObjCDirect -verify-additional-prefix warn-

//--- Gauge.h

#import <Foundation/Foundation.h>

@interface Gauge : NSObject
@property (direct) NSInteger level;
@end

//--- main.swift

import Foundation

final class Child: NSObject {
  @objc dynamic var value = 0
}

final class Model: NSObject {
  @objcDirect var count = 0
  @objcDirect var child = Child()
  @objc dynamic var observable = 0
  @objc var plain = 0
}

func observeModel(_ m: Model, _ g: Gauge) {
  _ = m.observe(\.count, options: [.new]) { _, _ in }
  // expected-error@-1 {{passing reference to direct property 'count' to KVO method 'observe(_:options:changeHandler:)' always traps at runtime, because a key path through a direct property has no key-value coding string}}

  // A direct property anywhere in the path takes the string away.
  _ = m.observe(\.child.value, options: [.new]) { _, _ in }
  // expected-error@-1 {{passing reference to direct property 'child' to KVO method 'observe(_:options:changeHandler:)' always traps at runtime}}

  _ = g.observe(\.level, options: [.new]) { _, _ in }
  // expected-error@-1 {{passing reference to direct property 'level' to KVO method 'observe(_:options:changeHandler:)' always traps at runtime}}

  _ = m.observe(\.observable, options: [.new]) { _, _ in }
}

@_semantics("keypath.mustBeValidForKVO")
func watch<T, V>(_ object: T, _ keyPath: KeyPath<T, V>) {}

func viaSemanticsAttribute(_ m: Model) {
  watch(m, \.count)
  // expected-error@-1 {{passing reference to direct property 'count' to KVO method 'watch' always traps at runtime}}
}

func nested(_ m: Model, _ tokens: inout [NSKeyValueObservation]) {
  tokens.append(m.observe(\.count, options: [.new]) { _, _ in })
  // expected-error@-1 {{passing reference to direct property 'count' to KVO method 'observe(_:options:changeHandler:)' always traps at runtime}}
  m.observe(\.count) { _, _ in }.invalidate()
  // expected-error@-1 {{passing reference to direct property 'count' to KVO method 'observe(_:options:changeHandler:)' always traps at runtime}}
  let later = {
    print("observing")
    _ = m.observe(\.count) { _, _ in }
    // expected-error@-1 {{passing reference to direct property 'count' to KVO method 'observe(_:options:changeHandler:)' always traps at runtime}}
  }
  later()

  _ = m.observe((\.count)) { _, _ in }
  // expected-error@-1 {{passing reference to direct property 'count' to KVO method 'observe(_:options:changeHandler:)' always traps at runtime}}
  _ = m.observe(\.count as KeyPath<Model, Int>) { _, _ in }
  // expected-error@-1 {{passing reference to direct property 'count' to KVO method 'observe(_:options:changeHandler:)' always traps at runtime}}
}

// The non-'@objc dynamic' warning keeps the reach it always had: a call
// outside any other call, with a bare key path literal.
func legacyWarningReach(_ m: Model, _ tokens: inout [NSKeyValueObservation]) {
  _ = m.observe(\.plain) { _, _ in }
  // expected-warning@-1 {{passing reference to non-'@objc dynamic' property 'plain' to KVO method 'observe(_:options:changeHandler:)' may lead to unexpected behavior or runtime trap}}
  tokens.append(m.observe(\.plain) { _, _ in })
  m.observe(\.plain) { _, _ in }.invalidate()
  _ = m.observe((\.plain)) { _, _ in }
}

// Only the direct property is diagnosed: these APIs don't need KVO compliance.
func keyPathStringAPIs(_ m: Model) {
  _ = NSSortDescriptor(keyPath: \Model.count, ascending: true)
  // expected-error@-1 {{passing reference to direct property 'count' to key-value coding API 'init(keyPath:ascending:)' always traps at runtime}}
  _ = NSExpression(forKeyPath: \Model.count)
  // expected-error@-1 {{passing reference to direct property 'count' to key-value coding API 'init(forKeyPath:)' always traps at runtime}}
  _ = m.publisher(for: \.count)
  // expected-error@-1 {{passing reference to direct property 'count' to KVO method 'publisher(for:options:)' always traps at runtime}}
  m.willChangeValue(for: \.count)
  // expected-error@-1 {{passing reference to direct property 'count' to KVO method 'willChangeValue(for:)' always traps at runtime}}

  _ = NSSortDescriptor(keyPath: \Model.observable, ascending: true)
  _ = m.publisher(for: \.observable)
}

//--- Lib.swift

import Foundation

public final class LibModel: NSObject {
  @objcDirect public var count = 0
}

//--- client.swift

import Foundation
import Lib

// Without the feature, a key path through a direct property is just a warning.
func observeLibrary(_ m: LibModel) {
  _ = m.observe(\.count, options: [.new]) { _, _ in }
  // expected-warn-warning@-1 {{passing reference to direct property 'count' to KVO method 'observe(_:options:changeHandler:)' does not work at runtime, because key-value coding cannot find its accessors}}
  // expected-warning@-2 {{passing reference to non-'@objc dynamic' property 'count' to KVO method 'observe(_:options:changeHandler:)' may lead to unexpected behavior or runtime trap}}
}

func observeImported(_ g: Gauge) {
  _ = g.observe(\.level, options: [.new]) { _, _ in }
  // expected-warn-warning@-1 {{passing reference to direct property 'level' to KVO method 'observe(_:options:changeHandler:)' does not work at runtime, because key-value coding cannot find its accessors}}
}
