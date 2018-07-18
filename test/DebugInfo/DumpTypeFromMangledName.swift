// RUN: %empty-directory(%t)

// %t.input: "A ---> B" ==> "A"
// RUN: sed -ne '/--->/s/ *--->.*$//p' < %S/Inputs/type-reconstr-names.txt > %t.input

// %t.check: "A ---> B" ==> "B"
// RUN: sed -ne '/--->/s/^.*---> *//p' < %S/Inputs/type-reconstr-names.txt > %t.check

// RUN: %target-build-swift -emit-executable %s -g -o %t/TypeReconstr -emit-module
// RUN: %lldb-moduleimport-test %t/TypeReconstr \
// RUN:   -type-from-mangled=%t.input > %t.output 2>&1
// RUN: diff %t.check %t.output

// REQUIRES: executable_test
extension Collection where Element: Equatable {
  func split<C: Collection>(separatedBy separator: C) -> [SubSequence]
    where C.Element == Element {
      var results = [SubSequence]()
      return results
  }
  func foo(_ x: Iterator.Element) {
    print(x)
  }
}

class Foo<T> {
  var x : T
  init(_ x : T) {
    self.x = x
  }
}

typealias Patatino<T> = Foo<T>

public struct Outer<T> {
  public struct Inner { }

  public typealias Foo<U> = Outer<U>.Inner

  public func blah() {
    let foo: Foo<Int> = Outer<Int>.Inner()
  }
}

func main() -> Int {
  struct patatino {}
  var p : Patatino<Int> = Patatino(23);
  return 0
}

let _ = main()

public struct tinky : Equatable, Hashable {
  internal let _value: Int

  public var hashValue: Int {
    return 0
  }
}

public func == (lhs: tinky, rhs: tinky) -> Bool {
  return true
}

public typealias patatino = UnsafePointer<tinky>
var local_thread_one: patatino?
