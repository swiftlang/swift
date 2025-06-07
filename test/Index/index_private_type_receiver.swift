// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/SDK)
// RUN: split-file %s %t
//
// RUN: mkdir -p %t/SDK/Frameworks/FakeSystem.framework/Modules/FakeSystem.swiftmodule
// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name FakeSystem \
// RUN:     -o %t/SDK/Frameworks/FakeSystem.framework/Modules/FakeSystem.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -swift-version 5 \
// RUN:     %t/FakeSystem.swift
//
// RUN: %empty-directory(%t/idx)
// RUN: %empty-directory(%t/modulecache)
//
// --- Built with indexing
// RUN: %target-swift-frontend \
// RUN:     -typecheck \
// RUN:     -index-system-modules \
// RUN:     -index-ignore-stdlib \
// RUN:     -index-store-path %t/idx \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     %t/view.swift
//
// --- Check the index.
// RUN: c-index-test core -print-record %t/idx | %FileCheck %s
//

//--- FakeSystem.swift
public protocol View {
}

public protocol ViewModifier {
  associatedtype Body: View
  typealias Content = _HiddenView<Self>
  func body(content: Self.Content) -> Self.Body
}

public struct _HiddenView<M>: View where M: ViewModifier {
  init(m: M) {}
}

public struct _HiddenModifier: ViewModifier {
  public func body(content: Content) -> some View {
    return _HiddenView(m: self)
  }
}

extension View {
  public func background() -> some View {
    return _HiddenView(m: _HiddenModifier())
  }
}

//--- view.swift
import FakeSystem

private struct Mod: FakeSystem.ViewModifier {
  func body(content: Content) -> some View {
    content
      .background()
    // CHECK: 6:8 | instance-method/Swift | s:10FakeSystem4ViewPAAE10backgroundQryF | Ref,Call,Dyn{{.*}}
  }
}
