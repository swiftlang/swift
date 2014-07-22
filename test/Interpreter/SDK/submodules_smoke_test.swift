// RUN: %target-build-swift -parse %s -Xfrontend -verify

import OpenAL.AL

let _ = alGetError
let _ = OpenAL.alGetError

let _ = alcCreateContext // expected-error{{use of unresolved identifier 'alcCreateContext'}}

#if os(iOS)
import UIKit.UIGestureRecognizerSubclass
let _: UIGestureRecognizer -> () -> Void = UIGestureRecognizer.reset

#elseif os(OSX)
import OpenGL.GL3
let _ = glGetString
let _ = OpenGL.glGetString

import AppKit.NSPanGestureRecognizer
typealias PanRecognizer = NSPanGestureRecognizer
typealias PanRecognizer2 = AppKit.NSPanGestureRecognizer

#endif

let _ = glVertexPointer // expected-error{{use of unresolved identifier 'glVertexPointer'}}
