// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action extract-imports %t/input.swift > %t/output.swift
// RUN: %diff %t/output.swift %t/expected.swift

//--- input.swift
import Foundation

#if canImport(UIKit)
import UIKit
public func uiKitFunc() {}
#endif

#if canImport(AppKit) && os(macOS)
import AppKit
public func appKitFunc() {}
#endif

#if !canImport(WatchKit)
import CoreGraphics
public func noWatchFunc() {}
#endif

#if os(macOS)
import Darwin
public func darwinFunc() {}
#elseif os(Linux)
import Glibc
#else
public func otherFunc() {}
#endif

#if DEBUG
public func debugFunc() {}
#endif

#if os(iOS)
public struct Foo {}
#endif
//--- expected.swift
import Foundation

#if canImport(UIKit)
import UIKit
#endif

#if canImport(AppKit) && os(macOS)
import AppKit
#endif

#if !canImport(WatchKit)
import CoreGraphics
#endif

#if os(macOS)
import Darwin
#elseif os(Linux)
import Glibc
#else
#endif
