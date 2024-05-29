// RUN: %target-swift-emit-irgen                             \
// RUN:     -parse-as-library                                \
// RUN:     -enable-builtin-module                           \
// RUN:     %s                                               \
// RUN: |                                                    \
// RUN: %FileCheck %s --check-prefix=CHECK-IR
// RUN: %target-run-simple-swift(-parse-as-library -enable-builtin-module -Xfrontend -sil-verify-all) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -parse-as-library -enable-builtin-module -Xfrontend -sil-verify-all) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -parse-as-library -enable-builtin-module -Xfrontend -sil-verify-all -Xfrontend -enable-ossa-modules) | %FileCheck %s

// REQUIRES: executable_test

// CHECK-IR-NOT: @"$sBpWV"

// CHECK: hi end
// CHECK: hi 3
// CHECK: hi 2
// CHECK: bye 2
// CHECK: bye 3
// CHECK: bye end
@main struct App { static func main() {
  let l: List<Int> = .more(
    2, 
    Box(
      .more(3, 
        Box(
          .end
        )
      )
    )
  )
  l.dump(prefix: "hi")
  l.dump(prefix: "bye")
}}

struct Box<Wrapped: ~Copyable & Dumpable>: ~Copyable {
  let ptr: MyLittlePointer<Wrapped>
  
  init(_ wrapped: consuming Wrapped) {
    wrapped.dump(prefix: "hi")
    ptr = .allocate(capacity: 1)
    ptr.initialize(to: wrapped)
  }
      
  deinit {
    ptr.move().dump(prefix: "bye")
    ptr.deallocate()
  }
}

enum List<Element>: ~Copyable, Dumpable {
    case end
    case more(Element, Box<List<Element>>)
    func dump(prefix: String) {
      switch self {
      case .more(let element, _):
        print(prefix, element)
      case .end:
        print(prefix, "end")
      }
    }
}

protocol Dumpable : ~Copyable {
  func dump(prefix: String)
}


import Builtin

@frozen
public enum MyLittleLayout<T : ~Copyable> {
  @_transparent
  public static var size: Int {
    return Int(Builtin.sizeof(T.self))
  }
  @_transparent
  public static var stride: Int {
    return Int(Builtin.strideof(T.self))
  }
}

struct MyLittlePointer<Pointee : ~Copyable> : Copyable {
  public let _rawValue: Builtin.RawPointer

  @_transparent
  public init(_ _rawValue: Builtin.RawPointer) {
    self._rawValue = _rawValue
  }

  @inlinable
  public static func allocate(capacity count: Int)
    -> MyLittlePointer<Pointee> {
    let size = MyLittleLayout<Pointee>.stride * count
    let align = (0)._builtinWordValue
    let rawPtr = Builtin.allocRaw(size._builtinWordValue, align)
    Builtin.bindMemory(rawPtr, count._builtinWordValue, Pointee.self)
    return MyLittlePointer(rawPtr)
  }

  @inlinable
  public func deallocate() {
    Builtin.deallocRaw(_rawValue, (-1)._builtinWordValue, (0)._builtinWordValue)
  }

  @inlinable
  public func initialize(to value: consuming Pointee) {
    Builtin.initialize(value, self._rawValue)
  }
  @inlinable
  public func deinitialize(count: Int) {
    Builtin.destroyArray(Pointee.self, _rawValue, count._builtinWordValue)
  }
  @inlinable
  public func move() -> Pointee {
    return Builtin.take(_rawValue)
  }
}
