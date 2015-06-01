// Also run this test in optimize test modes.
// REQUIRES: optimize_test

// RUN: %target-build-swift -parse %s -Xfrontend -verify
// RUN: %target-build-swift -emit-ir -g %s -DNO_ERROR > /dev/null

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import OpenGL.GL3
_ = glGetString
_ = OpenGL.glGetString

import AppKit.NSPanGestureRecognizer

@available(OSX, introduced=10.10)
typealias PanRecognizer = NSPanGestureRecognizer
typealias PanRecognizer2 = AppKit.NSPanGestureRecognizer

#if !NO_ERROR
_ = glVertexPointer // expected-error{{use of unresolved identifier 'glVertexPointer'}}
#endif
