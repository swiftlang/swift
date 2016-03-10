public protocol Prot {}

public struct Yoda<
  Base : IteratorProtocol
> : Prot {
    public func down<U>(
      p: Array<U>
    ) -> Array<U> {
        return p
    }
}

// RUN: %target-swift-ide-test -print-swift-file-interface -print-original-source -source-filename %s > %t.out
// RUN: diff -u %s.result %t.out
