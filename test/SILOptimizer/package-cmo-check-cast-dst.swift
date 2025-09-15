// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Verify a function with a cast instruction with an internal decl as dst
/// does not get serialized with PackageCMO in both scenarios below.

/// Scenario 1.
// RUN: %target-swift-frontend -emit-sil %t/Lib.swift -package-name pkg \
// RUN:   -wmo -allow-non-resilient-access -package-cmo \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -Xllvm -sil-print-function=topFunc -o %t/Lib.sil
// RUN: %FileCheck %s < %t/Lib.sil

/// Scenario 2.
// RUN: %target-build-swift %t/Mod.swift \
// RUN: -module-name=Mod -package-name pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Mod.swiftmodule -I%t \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -O -wmo -enable-library-evolution
// RUN: %target-sil-opt -enable-sil-verify-all %t/Mod.swiftmodule -o %t/Mod.sil
// RUN: %FileCheck %s --check-prefix=CHECK-MOD < %t/Mod.sil


//--- Lib.swift
public class Pub {
  public var pubVar: Int
  public init(_ arg: Int) {
    pubVar = arg
  }
}

class InternalKlass: Pub {}

/// Verify that `InternalKlass` is visited and the instruction containing it is not serialized.
// CHECK: sil @$s3Lib7topFuncySiAA3PubCF : $@convention(thin) (@guaranteed Pub) -> Int {
// CHECK:   checked_cast_br Pub in %0 to InternalKlass
public func topFunc(_ arg: Pub) -> Int {
  let x = arg as? InternalKlass
  return x != nil ? 1 : 0
}


//--- Mod.swift
struct SymmetricTextChildQuery<Provider: PubProto> {
  var source: Text
  init(_ arg: Text) {
    source = arg
  }
  /// This function references isCollapsible(), which contains an internal decl.
  /// If isCollapsible() were serialized, building a client of this module would fail
  /// due to a linker error: undefined symbol `CollapsibleTextModifier`.
  mutating func updateValue() {
    let resolvedSource = ResolvedStyledText.styledText(canCollapse: source.isCollapsible())
  }
}

@frozen
public struct Text: Equatable, Sendable {
  public init() {}
  @frozen
  @usableFromInline
  package enum Modifier: Equatable {
    case font
    case anyTextModifier(AnyTextModifier)
    
    @usableFromInline
    package static func ==(lhs: Modifier, rhs: Modifier) -> Bool {
      return true
    }
  }
  
  @usableFromInline
  package var modifiers = [Modifier]()
  
}

extension Text {
  /// Verify that function containing an internal decl CollapsibleTextModifier is
  /// not serialized with Package CMO.
  // CHECK-MOD-NOT: sil package [serialized_for_package] [canonical] @$s3Mod4TextV13isCollapsibleSbyF : $@convention(method) (@guaranteed Text) -> Bool {
  // CHECK-MOD-NOT: checked_cast_br AnyTextModifier in {{.*}} : $AnyTextModifier to CollapsibleTextModifier
  package func isCollapsible() -> Bool {
    modifiers.contains { modifier in
      guard case .anyTextModifier(let anyModifier) = modifier
      else { return false }
      return anyModifier is CollapsibleTextModifier
    }
  }
}

final class CollapsibleTextModifier: AnyTextModifier {
  override func isEqual(to other: AnyTextModifier) -> Bool {
    other is CollapsibleTextModifier
  }
}

public protocol PubProto {
  var pubVar: String { get set }
}

public struct PubStruct {
  public static func makeView<P: PubProto>(_ type: P.Type, _ arg: Text) {
    var child = SymmetricTextChildQuery<P>(arg)
    child.updateValue()
  }
}

public class ResolvedStyledText {
  package var canCollapse: Bool
  package init(_ arg: Bool) {
    canCollapse = arg
  }
}

extension ResolvedStyledText {
  package static func styledText(canCollapse: Bool) -> ResolvedStyledText {
    return ResolvedStyledText(canCollapse)
  }
}

@usableFromInline
package class AnyTextModifier {
  func isEqual(to other: AnyTextModifier) -> Bool { fatalError() }
}
