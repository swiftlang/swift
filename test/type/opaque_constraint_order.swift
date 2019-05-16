// RUN: %target-swift-frontend -disable-availability-checking -emit-ir -verify %s

// rdar://problem/50309983, make sure that the generic signature built for
// an opaque return type is correct when there are associated type constraints
// on the enclosing generic context

protocol Bort {}

extension Int: Bort {}

struct Butt<T: RandomAccessCollection>
  where T.Element: Bort, T.Index: Hashable
{
  func foo() -> some Bort {
    return 0
  }
}
