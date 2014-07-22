// RUN: %target-build-swift

import OpenGL.GL3

let _ = glGetString
let _ = OpenGL.glGetString

#if os(iOS)
import UIKit.UIGestureRecognizerSubclass
let _: UIGestureRecognizer -> () -> Void = UIGestureRecognizer.reset

#elseif os(OSX)
import AppKit.NSPanGestureRecognizer
typealias PanRecognizer = NSPanGestureRecognizer
typealias PanRecognizer2 = AppKit.NSPanGestureRecognizer

#endif
