// RUN: %target-build-swift -parse %s -Xfrontend -verify

#if os(iOS)
// FIXME: iOS submodule test

#elseif os(OSX)
import OpenGL.GL3
let _ = glGetString
let _ = OpenGL.glGetString

#endif

let _ = glVertexPointer // expected-error{{use of unresolved identifier 'glVertexPointer'}}


#if os(iOS)
import UIKit.UIGestureRecognizerSubclass
let _: UIGestureRecognizer -> () -> Void = UIGestureRecognizer.reset

#elseif os(OSX)
import AppKit.NSPanGestureRecognizer
typealias PanRecognizer = NSPanGestureRecognizer
typealias PanRecognizer2 = AppKit.NSPanGestureRecognizer

#endif
