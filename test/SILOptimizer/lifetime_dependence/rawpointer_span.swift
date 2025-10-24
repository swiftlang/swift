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
@_silgen_name("getRawPointer")
func getRawPointer() -> UnsafeRawPointer

@safe
@_silgen_name("getMutRawPointer")
func getMutRawPointer() -> UnsafeMutableRawPointer

func useSpan(_: RawSpan) {}

//===----------------------------------------------------------------------===//
// raw pointer .load()
//===----------------------------------------------------------------------===//

@available(Span 0.1, *)
@_lifetime(immortal)
func badLoad() -> RawSpan {
  let p = getRawPointer()
  return unsafe p.load(as: RawSpan.self) // expected-error{{lifetime-dependent value escapes its scope}}
    // expected-note@-2{{it depends on the lifetime of variable 'p'}}
    // expected-note@-2{{this use causes the lifetime-dependent value to escape}}
}

@available(Span 0.1, *)
@_lifetime(borrow p)
func goodLoad(p: UnsafeRawPointer) -> RawSpan {
  unsafe useSpan(p.load(as: RawSpan.self))
  return unsafe p.load(as: RawSpan.self)
}

//===----------------------------------------------------------------------===//
// raw pointer .loadUnaligned()
//
// TODO: test non-BitwiseCopyable loadUnaligned
//===----------------------------------------------------------------------===//

@available(Span 0.1, *)
@_lifetime(immortal)
func badBitwiseLoadUnaligned() -> RawSpan {
  let p = getRawPointer()
  return unsafe p.loadUnaligned(as: RawSpan.self) // expected-error{{lifetime-dependent value escapes its scope}}
    // expected-note@-2{{it depends on the lifetime of variable 'p'}}
    // expected-note@-2{{this use causes the lifetime-dependent value to escape}}
}

@available(Span 0.1, *)
@_lifetime(borrow p)
func goodBitwiseLoadUnaligned(p: UnsafeRawPointer) -> RawSpan {
  unsafe useSpan(p.loadUnaligned(as: RawSpan.self))
  return unsafe p.loadUnaligned(as: RawSpan.self)
}

/* TODO: support loadUnaligned<T: ~BitwiseCopyable>
@_lifetime(immortal)
func badGenericLoadUnaligned<T: ~Escapable>(p: UnsafeRawPointer, _: T.Type) -> T {
  let p = getRawPointer()
  return unsafe p.loadUnaligned(as: T.self) // ERROR
}

@_lifetime(borrow p)
func goodGenericLoadUnaligned<T: ~Escapable>(p: UnsafeRawPointer, _: T.Type) -> T {
  return unsafe p.loadUnaligned(as: T.self) // OK
}
*/

//===----------------------------------------------------------------------===//
// raw pointer .storeBytes()
//===----------------------------------------------------------------------===//

@available(Span 0.1, *)
func storeSpan(span: RawSpan) {
  let p = getMutRawPointer()
  unsafe p.storeBytes(of: span, as: RawSpan.self)
}

/* TODO: support storeBytes<T: ~BitwiseCopyable>
func storeGeneric<T: ~Escapable>(value: T) {
  let p = getMutRawPointer()
  unsafe p.storeBytes(of: value, as: T.self)
}
*/
