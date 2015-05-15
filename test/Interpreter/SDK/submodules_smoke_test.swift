// RUN: %target-build-swift -parse %s -Xfrontend -verify
// RUN: %target-build-swift -emit-ir -g %s -DNO_ERROR > /dev/null

// REQUIRES: objc_interop

import OpenAL.AL

_ = alGetError
_ = OpenAL.alGetError

#if !NO_ERROR
_ = alcCreateContext // expected-error{{use of unresolved identifier 'alcCreateContext'}}
#endif

#if os(OSX)
import OpenGL.GL3
_ = glGetString
_ = OpenGL.glGetString

import AppKit.NSPanGestureRecognizer

@available(OSX, introduced=10.10)
typealias PanRecognizer = NSPanGestureRecognizer
typealias PanRecognizer2 = AppKit.NSPanGestureRecognizer

#endif

#if !NO_ERROR
_ = glVertexPointer // expected-error{{use of unresolved identifier 'glVertexPointer'}}
#endif
