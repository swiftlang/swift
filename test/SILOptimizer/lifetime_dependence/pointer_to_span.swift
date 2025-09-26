// RUN: %target-swift-frontend -primary-file %s -parse-as-library -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -define-availability "Span 0.1:macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, visionOS 9999" \
// RUN:   -strict-memory-safety \
// RUN:   -enable-experimental-feature Lifetimes

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes

@safe
func getRawPointer() -> UnsafeRawPointer {
  UnsafeRawPointer(UnsafeMutablePointer<RawSpan>.allocate(capacity: 1))
}

@safe
func getMutRawPointer() -> UnsafeMutableRawPointer {
  UnsafeMutableRawPointer(UnsafeMutablePointer<RawSpan>.allocate(capacity: 1))
}

@safe
func getPointerToSpan() -> UnsafePointer<RawSpan> {
  unsafe UnsafePointer(UnsafeMutablePointer<RawSpan>.allocate(capacity: 1))
}

@safe
func getMutPointerToSpan() -> UnsafeMutablePointer<RawSpan> {
  UnsafeMutablePointer<RawSpan>.allocate(capacity: 1)
}

@safe
func getBufferPointerToSpan() -> UnsafeBufferPointer<RawSpan> {
  UnsafeBufferPointer(UnsafeMutableBufferPointer<RawSpan>.allocate(capacity: 1))
}

@safe
func getMutBufferPointerToSpan() -> UnsafeMutableBufferPointer<RawSpan> {
  UnsafeMutableBufferPointer<RawSpan>.allocate(capacity: 1)
}

@available(Span 0.1, *)
func useSpan(_ span: RawSpan) {}

//===----------------------------------------------------------------------===//
// raw pointer .initialize()
//===----------------------------------------------------------------------===//

@available(Span 0.1, *)
func goodInitializeRawLocal(array: [Int]) {
  let p = getMutRawPointer()
  unsafe p.initializeMemory(as: RawSpan.self, to: array.span.bytes)
  unsafe useSpan(p.load(as: RawSpan.self))
}

@available(Span 0.1, *)
func goodInitializeRawArg(array: [Int], p: UnsafeMutableRawPointer) {
  unsafe p.initializeMemory(as: RawSpan.self, to: array.span.bytes)
  unsafe useSpan(p.load(as: RawSpan.self))
}

//===----------------------------------------------------------------------===//
// .pointee
//===----------------------------------------------------------------------===//

// TODO: innaccurate diagnostic: 'p' is not itself lifetime-dependent.
@available(Span 0.1, *)
@_lifetime(immortal)
func badPointeeGet() -> RawSpan {
  let p = getPointerToSpan() // expected-error{{lifetime-dependent variable 'p' escapes its scope}}
    // expected-note@-1{{it depends on the lifetime of variable 'p'}}
  return unsafe p.pointee // expected-note{{this use causes the lifetime-dependent value to escape}}
}

@available(Span 0.1, *)
@_lifetime(borrow p)
func goodPointeeGet(p: UnsafePointer<RawSpan>) -> RawSpan {
  unsafe useSpan(p.pointee)
  return unsafe p.pointee
}

@available(Span 0.1, *)
@_lifetime(immortal)
func badMutPointeeGet() -> RawSpan {
  let p = getMutPointerToSpan() // expected-error{{lifetime-dependent variable 'p' escapes its scope}}
    // expected-note@-1{{it depends on the lifetime of variable 'p'}}
  return unsafe p.pointee // expected-note{{this use causes the lifetime-dependent value to escape}}
}

@available(Span 0.1, *)
func goodMutPointeeGet(p: UnsafeMutablePointer<RawSpan>) {
  unsafe useSpan(p.pointee)
}

@available(Span 0.1, *)
func mutPointeeSet(array: [Int], p: UnsafeMutablePointer<RawSpan>) {
  unsafe p.pointee = array.span.bytes
}

//===----------------------------------------------------------------------===//
// pointer subscript
//===----------------------------------------------------------------------===//

@available(Span 0.1, *)
@_lifetime(immortal)
func badSubscriptGet() -> RawSpan {
  let p = getPointerToSpan() // expected-error{{lifetime-dependent variable 'p' escapes its scope}}
    // expected-note@-1{{it depends on the lifetime of variable 'p'}}
  return unsafe p[0] // expected-note{{this use causes the lifetime-dependent value to escape}}
}

@available(Span 0.1, *)
@_lifetime(borrow p)
func goodSubscriptGet(p: UnsafePointer<RawSpan>) -> RawSpan {
  unsafe useSpan(p[0])
  return unsafe p[0]
}

@available(Span 0.1, *)
@_lifetime(immortal)
func badMutSubscriptGet() -> RawSpan {
  let p = getMutPointerToSpan() // expected-error{{lifetime-dependent variable 'p' escapes its scope}}
    // expected-note@-1{{it depends on the lifetime of variable 'p'}}
  return unsafe p[0] // expected-note{{this use causes the lifetime-dependent value to escape}}
}

@available(Span 0.1, *)
func goodMutSubscriptGet(p: UnsafeMutablePointer<RawSpan>) {
  unsafe useSpan(p[0])
}

@available(Span 0.1, *)
func mutSubscriptSet(array: [Int], p: UnsafeMutablePointer<RawSpan>) {
  unsafe p[0] = array.span.bytes
}

//===----------------------------------------------------------------------===//
// initialize
//===----------------------------------------------------------------------===//

@available(Span 0.1, *)
func goodInitializeLocal(array: [Int]) {
  let p = getMutPointerToSpan()
  unsafe p.initialize(to: array.span.bytes)
  unsafe useSpan(p[0])
}

@available(Span 0.1, *)
func goodInitializeArg(array: [Int], p: UnsafeMutablePointer<RawSpan>) {
  unsafe p.initialize(to: array.span.bytes)
  unsafe useSpan(p[0])
}

//===----------------------------------------------------------------------===//
// move
//===----------------------------------------------------------------------===//

@available(Span 0.1, *)
@_lifetime(immortal)
func badMove() -> RawSpan {
  let p = getMutPointerToSpan()
  return unsafe p.move() // expected-error{{lifetime-dependent value escapes its scope}}
    // expected-note@-2{{it depends on the lifetime of variable 'p'}}
    // expected-note@-2{{this use causes the lifetime-dependent value to escape}}
}

@available(Span 0.1, *)
@_lifetime(borrow p)
func goodMove(p: UnsafeMutablePointer<RawSpan>) -> RawSpan {
  unsafe p.move()
}

//===----------------------------------------------------------------------===//
// buffer subscript
//===----------------------------------------------------------------------===//

@available(Span 0.1, *)
@_lifetime(immortal)
func badBufferSubscriptGet() -> RawSpan {
  let buf = getBufferPointerToSpan() // expected-error{{lifetime-dependent variable 'buf' escapes its scope}}
    // expected-note@-1{{it depends on the lifetime of variable 'buf'}}
  return unsafe buf[0] // expected-note{{this use causes the lifetime-dependent value to escape}}
}

@available(Span 0.1, *)
@_lifetime(borrow buf)
func goodBufferSubscriptGet(buf: UnsafeBufferPointer<RawSpan>) -> RawSpan {
  unsafe useSpan(buf[0])
  return unsafe buf[0]
}

@available(Span 0.1, *)
@_lifetime(immortal)
func badMutBufferSubscriptGet() -> RawSpan {
  let buf = getMutBufferPointerToSpan() // expected-error{{lifetime-dependent variable 'buf' escapes its scope}}
    // expected-note@-1{{it depends on the lifetime of variable 'buf'}}
  return unsafe buf[0] // expected-note{{this use causes the lifetime-dependent value to escape}}
}

@available(Span 0.1, *)
func goodMutBufferSubscriptGet(buf: UnsafeMutableBufferPointer<RawSpan>) {
  unsafe useSpan(buf[0])
}

@available(Span 0.1, *)
func mutBufferSubscriptSet(array: [Int], buf: UnsafeMutableBufferPointer<RawSpan>) {
  unsafe buf[0] = array.span.bytes
}

//===----------------------------------------------------------------------===//
// moveElement
//===----------------------------------------------------------------------===//

@available(Span 0.1, *)
@_lifetime(immortal)
func badMoveElement() -> RawSpan {
  let buf = getMutBufferPointerToSpan()
  return unsafe buf.moveElement(from: 0) // expected-error{{lifetime-dependent value escapes its scope}}
    // expected-note@-2{{it depends on the lifetime of variable 'buf'}}
    // expected-note@-2{{this use causes the lifetime-dependent value to escape}}
}

@available(Span 0.1, *)
@_lifetime(borrow buf)
func goodMoveElement(buf: UnsafeMutableBufferPointer<RawSpan>) -> RawSpan {
  unsafe buf.moveElement(from: 0)
}
