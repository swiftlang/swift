// REQUIRES: OS=macosx
// RUN: %target-swift-frontend -typecheck %s -swift-version 3
// RUN: %target-swift-frontend -typecheck -update-code -primary-file %s -emit-migrated-file-path %t.result -swift-version 3 -disable-migrator-fixits
// RUN: diff -u %s.expected %t.result

import AppKit

func a(_ outlineView: NSOutlineView) {
  let cell: NSTableCellView = outlineView.make(withIdentifier: "HeaderCell", owner: outlineView) as! NSTableCellView
}

NSApplicationLoad()
NSBeep()

NSRectFill(NSRect.zero)
NSRectClip(NSRect.zero)
NSFrameRect(NSRect.zero)

// NSRect.zero.fill(using: NSCompositingOperation.clear)
NSRectFillUsingOperation(NSRect.zero, NSCompositingOperation.clear)

// NSRect.zero.frame(withWidth: 0.0)
NSFrameRectWithWidth(NSRect.zero, 0.0)

// NSRect.zero.frame(withWidth: 0.0, using: NSCompositingOperation.clear)
NSFrameRectWithWidthUsingOperation(NSRect.zero, 0.0, NSCompositingOperation.clear)

let isTrue = true

// (isTrue ? NSRect.zero : NSRect.zero).frame(withWidth: 0.0)
NSFrameRectWithWidth(isTrue ? NSRect.zero : NSRect.zero, 0.0)

var rekts = [NSRect.zero]
var kolors = [NSColor.red]
var grays = [CGFloat(0)]

// rekts.fill()
NSRectFillList(&rekts, 1)

// rekts.fill(using: NSCompositingOperation.clear)
NSRectFillListUsingOperation(rekts, 1, NSCompositingOperation.clear)

// rekts.clip()
NSRectClipList(&rekts, 1)

// TODO: zip2(rekts, kolors).fill()
// NSRectFillListWithColors(&rekts, &kolors, 1)

// TODO: zip2(rekts, kolors).fill(using: NSCompositingOperation.clear)
// NSRectFillListWithColorsUsingOperation(&rekts, &kolors, 1, NSCompositingOperation.clear)

// TODO: zip2(rekts, grays).fill()
// NSRectFillListWithGrays(&rekts, &grays, 1)

// TODO: NSAnimationEffect.poof.show(centeredAt: NSPoint.zero, size: NSSize.zero)
// NSShowAnimationEffect(NSAnimationEffect.poof, NSPoint.zero, NSSize.zero, nil, nil, nil)

// _ = NSWindow.Depth.availableDepths
_ = NSAvailableWindowDepths()

// TODO: _ = NSWindow.Depth.bestDepth("", 24, 0, false)
// _ = NSBestDepth("", 24, 0, false, nil)

var cacheSize: GLint = 0

// cacheSize = NSOpenGLGOFormatCacheSize.globalValue
NSOpenGLGetOption(NSOpenGLGOFormatCacheSize, &cacheSize)

// NSOpenGLGOFormatCacheSize.globalValue = 5
NSOpenGLSetOption(NSOpenGLGOFormatCacheSize, 5)

var major = GLint(0)
var minor = GLint(0)

// TODO: (major, minor) = NSOpenGLContext.openGLVersion
// NSOpenGLGetVersion(&major, &minor)
