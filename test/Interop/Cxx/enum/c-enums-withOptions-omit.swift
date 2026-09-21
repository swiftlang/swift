// RUN: %target-swift-ide-test -print-module -module-to-print=CenumsWithOptionsOmit -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s
// REQUIRES: objc_interop

import CenumsWithOptionsOmit

// CHECK: class NSSet {
// CHECK-NEXT: class func enumerateObjects(options
// CHECK-NEXT: func enumerateObjects(options
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "enumerateObjects(options:)")

// CHECK-NOT: typealias MacroPrefixEnum
// CHECK: struct MacroPrefixEnum : OptionSet

// CHECK: class TestsForEnhancedOmitNeedlessWords {

// Tests for withOptions -> 'with options'
// CHECK-NEXT:  class func difference(fromArray other: CInt, with options: NSOrderedCollectionDifferenceCalculationOptions = [])
// CHECK-NEXT:  func difference(fromArray other: CInt, with options: NSOrderedCollectionDifferenceCalculationOptions = [])
// CHECK-NEXT:  @available(swift, obsoleted: 3, renamed: "difference(fromArray:with:)")
// CHECK-NEXT: class func differenceFromArray(_ other: CInt, withOptions options: NSOrderedCollectionDifferenceCalculationOptions = [])
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "difference(fromArray:with:)")
// CHECK-NEXT: func differenceFromArray(_ other: CInt, withOptions options: NSOrderedCollectionDifferenceCalculationOptions = [])

// Tests for ofUnit -> 'of unit'
// CHECK-NEXT: class func minimumRange(of unit: NSCalendarUnit) -> CUnsignedInt
// CHECK-NEXT: func minimumRange(of unit: NSCalendarUnit) -> CUnsignedInt
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "minimumRange(of:)")
// CHECK-NEXT: class func minimumRangeOfUnit(_ unit: NSCalendarUnit) -> CUnsignedInt
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "minimumRange(of:)")
// CHECK-NEXT: func minimumRangeOfUnit(_ unit: NSCalendarUnit) -> CUnsignedInt

// Tests for inDomain -> 'in domain'
// CHECK-NEXT: class func url(forDirectory directory: CUnsignedInt, in domain: NSSearchPathDomainMask) -> CUnsignedInt
// CHECK-NEXT: func url(forDirectory directory: CUnsignedInt, in domain: NSSearchPathDomainMask) -> CUnsignedInt
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "url(forDirectory:in:)")
// CHECK-NEXT: class func URLForDirectory(_ directory: CUnsignedInt, inDomain domain: NSSearchPathDomainMask) -> CUnsignedInt
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "url(forDirectory:in:)")
// CHECK-NEXT: func URLForDirectory(_ directory: CUnsignedInt, inDomain domain: NSSearchPathDomainMask) -> CUnsignedInt

// Tests for shouldUseAction -> 'shouldUse action'
// CHECK-NEXT: class func layoutManager(_ layoutManager: CUnsignedInt, shouldUse action: NSControlCharacterAction) -> CUnsignedInt
// CHECK-NEXT: func layoutManager(_ layoutManager: CUnsignedInt, shouldUse action: NSControlCharacterAction) -> CUnsignedInt
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "layoutManager(_:shouldUse:)")
// CHECK-NEXT: class func layoutManager(_ layoutManager: CUnsignedInt, shouldUseAction action: NSControlCharacterAction) -> CUnsignedInt
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "layoutManager(_:shouldUse:)")
// CHECK-NEXT: func layoutManager(_ layoutManager: CUnsignedInt, shouldUseAction action: NSControlCharacterAction) -> CUnsignedInt

// Tests for forState -> 'for state'
// CHECK-NEXT: class func setBackButtonBackgroundImage(_ backgroundImage: CUnsignedInt, for state: UIControlState)
// CHECK-NEXT: func setBackButtonBackgroundImage(_ backgroundImage: CUnsignedInt, for state: UIControlState)
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "setBackButtonBackgroundImage(_:for:)")
// CHECK-NEXT: class func setBackButtonBackgroundImage(_ backgroundImage: CUnsignedInt, forState state: UIControlState)
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "setBackButtonBackgroundImage(_:for:)")
// CHECK-NEXT: func setBackButtonBackgroundImage(_ backgroundImage: CUnsignedInt, forState state: UIControlState)

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
