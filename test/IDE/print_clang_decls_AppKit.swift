// RUN: rm -rf %t
// RUN: mkdir -p %t

// This file deliberately does not use %clang-importer-sdk for most RUN lines.
// Instead, it generates custom overlay modules itself, and uses -I %t when it
// wants to use them.

// REQUIRES: OS=macosx

// RUN: %target-swift-frontend -emit-module -o %t %clang-importer-sdk %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN: %target-swift-frontend -emit-module -o %t %clang-importer-sdk %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend -emit-module -o %t -I %t %clang-importer-sdk %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN: %target-swift-frontend -emit-module -o %t -I %t %clang-importer-sdk %S/../Inputs/clang-importer-sdk/swift-modules/AppKit.swift

// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print=AppKit -sdk %S/../Inputs/clang-importer-sdk -I %t -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=APPKIT -strict-whitespace < %t.printed.txt

// APPKIT-LABEL: {{^}}extension NSString {{{$}}

// APPKIT-LABEL: {{^}}class NSView : NSObject, NSCoding, NSAccessibility, NSObjectProtocol {{{$}}
// APPKIT-NEXT: init?(coder aDecoder: NSCoder)
// APPKIT-NEXT: func isDescendantOf(aView: NSView) -> Bool
// APPKIT-NEXT: func ancestorSharedWithView(aView: NSView) -> NSView?
// APPKIT-NEXT: func addSubview(aView: NSView)
// APPKIT-NEXT: func addSubview(aView: NSView, positioned place: UInt32, relativeTo otherView: NSView?)
// APPKIT-NEXT: unowned(unsafe) var superview: @sil_unmanaged NSView? { get }
// APPKIT-NEXT: var layer: CALayer?
// APPKIT-NEXT: var trackingAreas: [AnyObject] { get }
// APPKIT-NEXT: var subviews: [AnyObject]
// APPKIT-LABEL:      extension NSView {
// APPKIT-NEXT:   unowned(unsafe) var nextKeyView: @sil_unmanaged NSView?

// APPKIT-LABEL: {{^}}class NSMenuItem : NSObject, NSCopying, NSCoding {
// APPKIT-NEXT: unowned(unsafe) var menu: @sil_unmanaged NSMenu?
// APPKIT-NEXT: var title: String
// APPKIT-NEXT: @NSCopying var attributedTitle: NSAttributedString?
// APPKIT-NEXT: weak var target: @sil_weak AnyObject
// APPKIT-NEXT: var action: Selector
// APPKIT: {{^}}}{{$}}
// APPKIT: let NSViewFrameDidChangeNotification: String
// APPKIT: let NSViewFocusDidChangeNotification: String


