// RUN: %empty-directory(%t/cache)
// RUN: %target-build-swift -typecheck %s -Xfrontend -verify -Xfrontend -verify-ignore-unknown -module-cache-path %t/cache
// RUN: %target-build-swift -emit-ir -g %s -DNO_ERROR -module-cache-path %t/cache > /dev/null
// REQUIRES: executable_test

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import OpenGL.GL3
_ = glGetString // expected-warning * {{'glGetString' was deprecated}}
_ = OpenGL.glGetString // expected-warning * {{'glGetString' was deprecated}}
_ = GL_COLOR_BUFFER_BIT
_ = OpenGL.GL_COLOR_BUFFER_BIT

import AppKit.NSPanGestureRecognizer

@available(OSX, introduced: 10.10)
typealias PanRecognizer = NSPanGestureRecognizer

@available(OSX, introduced: 10.10)
typealias PanRecognizer2 = AppKit.NSPanGestureRecognizer

#if !NO_ERROR
_ = glVertexPointer // expected-error{{cannot find 'glVertexPointer' in scope}}
#endif

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected warning produced: 'cacheParamsComputed' is deprecated
// <unknown>:0: error: unexpected warning produced: 'cacheAlphaComputed' is deprecated
// <unknown>:0: error: unexpected warning produced: 'keepCacheWindow' is deprecated
// <unknown>:0: error: unexpected error produced: 'memoryless' is unavailable
