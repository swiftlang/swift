// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize-source %t/input.swift > %t/output.swift
// RUN: %diff %t/output.swift %t/expected.swift

//--- input.swift
import Foundation

#if os(macOS)
public func macFunc() {
  print("mac")
}
private func macPrivate() {}
#elseif os(iOS)
public func iosFunc() {}
private func iosPrivate() {}
#else
public func otherFunc() {}
#endif

#if canImport(UIKit)
import UIKit
public func uiKitFunc() {}
private func uiKitPrivate() {}
#endif

#if canImport(SomeKit, _underlyingVersion: 8.0.4)
public func underlyingVersionFunc() {}
#endif

#if canImport(SomeKit, _version: 1.2.3.4)
public func someKitFunc() {}
#endif

#if DEBUG
public func debugFunc() {
  print("debug")
}
#if VERBOSE
public func verboseFunc() {}
private func debugPrivate() {}
#endif
#endif
//--- expected.swift
import Foundation

#if os(macOS)
public func macFunc()
private func macPrivate()
#elseif os(iOS)
public func iosFunc()
private func iosPrivate()
#else
public func otherFunc()
#endif

#if canImport(UIKit)
import UIKit
public func uiKitFunc()
private func uiKitPrivate()
#endif

#if canImport(SomeKit, _underlyingVersion: 8.0.4)
public func underlyingVersionFunc()
#endif

#if canImport(SomeKit, _version: 1.2.3.4)
public func someKitFunc()
#endif

#if DEBUG
public func debugFunc()
#if VERBOSE
public func verboseFunc()
private func debugPrivate()
#endif
#endif
