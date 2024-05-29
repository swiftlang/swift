// RUN: %target-swift-emit-ir \
// RUN:     %s \
// RUN:     -enable-builtin-module \
// RUN:     -sil-verify-all \
// RUN:     -verify

// Check that a simple version of the noncopyable linked list works through the
// SIL pipeline.

struct List<Element>: ~Copyable {
  struct Node: ~Copyable {
    var element: Element
    var next: Link
  }

  enum Link: ~Copyable {
    case empty
    case more(Box<Node>)
  }

  var head: Link = .empty

  deinit {
    dumpList(self)
    var head = self.head // self is immutable so a local variable must be introduced
    while case .more(let box) = consume head {
      head = box.move().next
    }
  }
}

func dumpList<Element>(_ l: borrowing List<Element>) {}

struct Box<Wrapped: ~Copyable>: ~Copyable {
  private let _pointer: MyLittlePointer<Wrapped>
  
  init(_ element: consuming Wrapped) {
    _pointer = .allocate(capacity: 1)
    _pointer.initialize(to: element)
  }
      
  deinit {
    _pointer.deinitialize(count: 1)
    _pointer.deallocate()
  }
  
  consuming func move() -> Wrapped {
    let wrapped = _pointer.move()
    _pointer.deallocate()
    discard self
    return wrapped
  }
}

// Standalone version of MemoryLayout + UnsafeMutablePointer with noncopyable T.

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

