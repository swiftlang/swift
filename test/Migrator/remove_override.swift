// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -update-code -swift-version 4 -disable-migrator-fixits -primary-file %s -emit-migrated-file-path %t/remove_override.result.swift -o %t/rename-func-decl.swift.remap
// RUN: diff -u %S/remove_override.swift.expected %t/remove_override.result.swift

import AppKit

class AppDelegate: NSObject {
  override class func application(_ sender: NSApplication, delegateHandlesKey key: String) -> Bool {
    super.application(sender, delegateHandlesKey: key)
    return false
  }
  override func application(_ sender: NSApplication, delegateHandlesKey key: String) -> Bool {
    return super.application(sender, delegateHandlesKey: key)
  }
  override class func changeColor(_ sender: Any?) {
    super.changeColor(sender)
  }
  override func changeColor(_ sender: Any?) {
  
  }
  override class func controlTextDidBeginEditing(_ obj: Notification) {
  
  }
  override func controlTextDidBeginEditing(_ obj: Notification) {
  
  }
  override class func controlTextDidEndEditing(_ obj: Notification) {
  
  }
  override func controlTextDidEndEditing(_ obj: Notification) {
  
  }
  override class func controlTextDidChange(_ obj: Notification) {
  
  }
  override func controlTextDidChange(_ obj: Notification) {
  
  }
  override class func changeFont(_ sender: Any?) {
  
  }
  override func changeFont(_ sender: Any?) {
  
  }
  override class func validModesForFontPanel(_ fontPanel: NSFontPanel) -> NSFontPanel.ModeMask {
    return []
  }
  override func validModesForFontPanel(_ fontPanel: NSFontPanel) -> NSFontPanel.ModeMask {
    return []
  }
  override class func discardEditing() {
  
  }
  override func discardEditing() {
  
  }
  override class func commitEditing() -> Bool {
    return false
  }
  override func commitEditing() -> Bool {
    return false
  }
  override class func commitEditing(withDelegate delegate: Any?, didCommit didCommitSelector: Selector?, contextInfo: UnsafeMutableRawPointer?) {
  
  }
  override func commitEditing(withDelegate delegate: Any?, didCommit didCommitSelector: Selector?, contextInfo: UnsafeMutableRawPointer?) {
  
  }
  override class func commitEditingAndReturnError() throws {
  
  }
  override func commitEditingAndReturnError() throws {
  
  }
  override class func objectDidBeginEditing(_ editor: Any) {
  
  }
  override func objectDidBeginEditing(_ editor: Any) {
  
  }
  override class func objectDidEndEditing(_ editor: Any) {
  
  }
  override func objectDidEndEditing(_ editor: Any) {
  
  }
  override class func validateMenuItem(_ menuItem: NSMenuItem) -> Bool {
    return false
  }
  override func validateMenuItem(_ menuItem: NSMenuItem) -> Bool {
    return false
  }
  override class func pasteboard(_ sender: NSPasteboard, provideDataForType type: NSPasteboard.PasteboardType) {
  
  }
  override func pasteboard(_ sender: NSPasteboard, provideDataForType type: NSPasteboard.PasteboardType) {
  
  }
  override class func pasteboardChangedOwner(_ sender: NSPasteboard) {
  
  }
  override func pasteboardChangedOwner(_ sender: NSPasteboard) {
  
  }
  override class func layer(_ layer: CALayer, shouldInheritContentsScale newScale: CGFloat, from window: NSWindow) -> Bool {
    return false
  }
  override func layer(_ layer: CALayer, shouldInheritContentsScale newScale: CGFloat, from window: NSWindow) -> Bool {
    return false
  }
  override class func view(_ view: NSView, stringForToolTip tag: NSView.ToolTipTag, point: NSPoint, userData data: UnsafeMutableRawPointer?) -> String {
    return ""
  }
  override func view(_ view: NSView, stringForToolTip tag: NSView.ToolTipTag, point: NSPoint, userData data: UnsafeMutableRawPointer?) -> String {
    return ""
  }
}

// We shouldn't migrate further sub-class.
class MyAppDelegate: AppDelegate {
  override func commitEditing() -> Bool {
    super.commitEditing()
    return false
  }
}

