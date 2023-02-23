// RUN: %target-swift-ide-test -print-module -module-to-print=CenumsWithOptionsOmit -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s
// REQUIRES: objc_interop

import CenumsWithOptionsOmit

// CHECK: class NSSet {
// CHECK-NEXT: class func enumerateObjects(options
// CHECK-NEXT: func enumerateObjects(options
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "enumerateObjects(options:)")

// CHECK: class TestsForEnhancedOmitNeedlessWords {

// Tests for withOptions -> 'with options'
// CHECK-NEXT:  class func difference(fromArray other: Int32, with options: NSOrderedCollectionDifferenceCalculationOptions = [])
// CHECK-NEXT:  func difference(fromArray other: Int32, with options: NSOrderedCollectionDifferenceCalculationOptions = [])
// CHECK-NEXT:  @available(swift, obsoleted: 3, renamed: "difference(fromArray:with:)")
// CHECK-NEXT: class func differenceFromArray(_ other: Int32, withOptions options: NSOrderedCollectionDifferenceCalculationOptions = [])
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "difference(fromArray:with:)")
// CHECK-NEXT: func differenceFromArray(_ other: Int32, withOptions options: NSOrderedCollectionDifferenceCalculationOptions = [])

// Tests for ofUnit -> 'of unit'
// CHECK-NEXT: class func minimumRange(of unit: NSCalendarUnit) -> UInt32
// CHECK-NEXT: func minimumRange(of unit: NSCalendarUnit) -> UInt32
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "minimumRange(of:)")
// CHECK-NEXT: class func minimumRangeOfUnit(_ unit: NSCalendarUnit) -> UInt32
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "minimumRange(of:)")
// CHECK-NEXT: func minimumRangeOfUnit(_ unit: NSCalendarUnit) -> UInt32

// Tests for inDomain -> 'in domain'
// CHECK-NEXT: class func url(forDirectory directory: UInt32, in domain: NSSearchPathDomainMask) -> UInt32
// CHECK-NEXT: func url(forDirectory directory: UInt32, in domain: NSSearchPathDomainMask) -> UInt32
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "url(forDirectory:in:)")
// CHECK-NEXT: class func URLForDirectory(_ directory: UInt32, inDomain domain: NSSearchPathDomainMask) -> UInt32
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "url(forDirectory:in:)")
// CHECK-NEXT: func URLForDirectory(_ directory: UInt32, inDomain domain: NSSearchPathDomainMask) -> UInt32

// Tests for shouldUseAction -> 'shouldUse action'
// CHECK-NEXT: class func layoutManager(_ layoutManager: UInt32, shouldUse action: NSControlCharacterAction) -> UInt32
// CHECK-NEXT: func layoutManager(_ layoutManager: UInt32, shouldUse action: NSControlCharacterAction) -> UInt32
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "layoutManager(_:shouldUse:)")
// CHECK-NEXT: class func layoutManager(_ layoutManager: UInt32, shouldUseAction action: NSControlCharacterAction) -> UInt32
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "layoutManager(_:shouldUse:)")
// CHECK-NEXT: func layoutManager(_ layoutManager: UInt32, shouldUseAction action: NSControlCharacterAction) -> UInt32

// Tests for forState -> 'for state'
// CHECK-NEXT: class func setBackButtonBackgroundImage(_ backgroundImage: UInt32, for state: UIControlState)
// CHECK-NEXT: func setBackButtonBackgroundImage(_ backgroundImage: UInt32, for state: UIControlState)
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "setBackButtonBackgroundImage(_:for:)")
// CHECK-NEXT: class func setBackButtonBackgroundImage(_ backgroundImage: UInt32, forState state: UIControlState)
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "setBackButtonBackgroundImage(_:for:)")
// CHECK-NEXT: func setBackButtonBackgroundImage(_ backgroundImage: UInt32, forState state: UIControlState)

// Tests for toState -> 'to state'
// CHECK-NEXT: class func willTransition(to state: UITableViewCellStateMask)
// CHECK-NEXT: func willTransition(to state: UITableViewCellStateMask)
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "willTransition(to:)")
// CHECK-NEXT: class func willTransitionToState(_ state: UITableViewCellStateMask)
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "willTransition(to:)")
// CHECK-NEXT: func willTransitionToState(_ state: UITableViewCellStateMask)

// Tests for forControlEvents -> 'for controlEvents'
// CHECK-NEXT: class func addTarget(_ target: Any?, action: OpaquePointer!, for controlEvents: UIControlEvents)
// CHECK-NEXT: func addTarget(_ target: Any?, action: OpaquePointer!, for controlEvents: UIControlEvents)
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "addTarget(_:action:for:)")
// CHECK-NEXT: class func addTarget(_ target: Any?, action: OpaquePointer!, forControlEvents controlEvents: UIControlEvents)
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "addTarget(_:action:for:)")
// CHECK-NEXT: func addTarget(_ target: Any?, action: OpaquePointer!, forControlEvents controlEvents: UIControlEvents)

// Tests for atScrollPosition -> 'at atScrollPosition'
// CHECK-NEXT: class func scrollToRow(at indexPath: NSIndexPath!, at scrollPosition: UITableViewScrollPosition)
// CHECK-NEXT: func scrollToRow(at indexPath: NSIndexPath!, at scrollPosition: UITableViewScrollPosition)
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "scrollToRow(at:at:)")
// CHECK-NEXT: class func scrollToRowAtIndexPath(_ indexPath: NSIndexPath!, atScrollPosition scrollPosition: UITableViewScrollPosition)
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "scrollToRow(at:at:)")
// CHECK-NEXT: func scrollToRowAtIndexPath(_ indexPath: NSIndexPath!, atScrollPosition scrollPosition: UITableViewScrollPosition)
