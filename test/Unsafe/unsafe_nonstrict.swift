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
