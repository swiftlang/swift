// RUN: %target-typecheck-verify-swift

@unsafe func unsafeFunc() { }

@unsafe
struct UnsafeType { }

protocol P { }

struct X: @unsafe P { }

func acceptP<T: P>(_: T) { }

func testItAll(ut: UnsafeType, x: X, i: Int) {
  _ = unsafe ut
  unsafe acceptP(x)
  _ = unsafe i // expected-warning{{no unsafe operations occur within 'unsafe' expression}}
}

func superDuperUnsafe(_ bytes: UnsafeRawBufferPointer) {
  // expected-warning@+1{{no unsafe operations occur within 'unsafe' expression}}
  let byte = unsafe unsafe bytes.first ?? 0
  _ = byte
  _ = unsafe bytes.first ?? 0
}
