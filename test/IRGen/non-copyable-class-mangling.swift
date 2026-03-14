// RUN: %target-swift-frontend %s -emit-ir -o /dev/null

// Check that we don't crash on this in the old re-mangler.
// rdar://133333754

struct S<T> where T: ~Copyable {
  final class C {
    let x = 27
  }
}

