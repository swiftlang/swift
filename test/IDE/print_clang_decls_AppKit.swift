// REQUIRES: OS=macosx

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -source-filename %s -module-to-print=AppKit -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: %FileCheck %s -check-prefix=APPKIT -strict-whitespace < %t.printed.txt

// APPKIT-LABEL: {{^}}extension NSString {{{$}}

// APPKIT-LABEL: {{^}}class NSView : NSObject, NSCoding, NSAccessibility {{{$}}
// APPKIT-NEXT: init?(coder aDecoder: NSCoder)
// APPKIT-NEXT: func isDescendant(of aView: NSView) -> Bool
// APPKIT-NEXT: @available(swift, obsoleted: 3, renamed: "isDescendant(of:)")
// APPKIT-NEXT: func isDescendantOf(_ aView: NSView) -> Bool
// APPKIT-NEXT: func ancestorShared(with aView: NSView) -> NSView?
// APPKIT-NEXT: @available(swift, obsoleted: 3, renamed: "ancestorShared(with:)")
// APPKIT-NEXT: func ancestorSharedWithView(_ aView: NSView) -> NSView?
// APPKIT-NEXT: func addSubview(_ aView: NSView)
// APPKIT-NEXT: func addSubview(_ aView: NSView, positioned place: UInt32, relativeTo otherView: NSView?)
// APPKIT-NEXT: unowned(unsafe) var superview: @sil_unmanaged NSView? { get }
// APPKIT-NEXT: var layer: CALayer?
// APPKIT-NEXT: var trackingAreas: [Any] { get }
// APPKIT-NEXT: var subviews: [Any]
// APPKIT-LABEL:      extension NSView {
// APPKIT-NEXT:   unowned(unsafe) var nextKeyView: @sil_unmanaged NSView?

// APPKIT-LABEL: {{^}}class NSMenuItem : NSObject, NSCopying, NSCoding {
// APPKIT-NEXT: unowned(unsafe) var menu: @sil_unmanaged NSMenu?
// APPKIT-NEXT: var title: String
// APPKIT-NEXT: @NSCopying var attributedTitle: NSAttributedString?
// APPKIT-NEXT: weak var target: @sil_weak AnyObject!
// APPKIT-NEXT: var action: Selector
// APPKIT: {{^}}}{{$}}
// APPKIT: extension NSNotification.Name {
// APPKIT:   static let NSViewFrameDidChange: NSNotification.Name
// APPKIT:   static let NSViewFocusDidChange: NSNotification.Name
// APPKIT: }

