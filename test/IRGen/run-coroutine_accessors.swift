// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                              \
// RUN:     %t/Library.swift                                \
// RUN:     -emit-module                                    \
// RUN:     -enable-library-evolution                       \
// RUN:     -module-name Library                            \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-build-swift                                 \
// RUN:     %t/Downstream.swift                             \
// RUN:     -c                                              \
// RUN:     -Onone                                          \
// RUN:     -parse-as-library                               \
// RUN:     -module-name main                               \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -lLibrary                                       \
// RUN:     -I %t                                           \
// RUN:     -o %t/Executable.o

// RUN: %target-build-swift-dylib(%t/%target-library-name(Library)) \
// RUN:     %t/Library.swift                                        \
// RUN:     -emit-module                                            \
// RUN:     -enable-library-evolution                               \
// RUN:     -enable-experimental-feature CoroutineAccessors         \
// RUN:     -emit-module-path %t/Library.swiftmodule                \
// RUN:     -module-name Library

// RUN: %target-build-swift   \
// RUN:     %t/Executable.o   \
// RUN:     -lLibrary         \
// RUN:     -L %t             \
// RUN:     %target-rpath(%t) \
// RUN:     -o %t/main

// RUN: %target-codesign %t/%target-library-name(Library)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(Library) | %FileCheck %s

// Now do it all again, but without using the runtime
// swift_task_dealloc_through function.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                                \
// RUN:     -Xllvm -enable-runtime-task-dealloc-through=false \
// RUN:     %t/Library.swift                                  \
// RUN:     -emit-module                                      \
// RUN:     -enable-library-evolution                         \
// RUN:     -module-name Library                              \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-build-swift                                   \
// RUN:     -Xllvm -enable-runtime-task-dealloc-through=false \
// RUN:     %t/Downstream.swift                               \
// RUN:     -c                                                \
// RUN:     -Onone                                            \
// RUN:     -parse-as-library                                 \
// RUN:     -module-name main                                 \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -lLibrary                                         \
// RUN:     -I %t                                             \
// RUN:     -o %t/Executable.o

// RUN: %target-build-swift-dylib(%t/%target-library-name(Library)) \
// RUN:     -Xllvm -enable-runtime-task-dealloc-through=false       \
// RUN:     %t/Library.swift                                        \
// RUN:     -emit-module                                            \
// RUN:     -enable-library-evolution                               \
// RUN:     -enable-experimental-feature CoroutineAccessors         \
// RUN:     -emit-module-path %t/Library.swiftmodule                \
// RUN:     -module-name Library

// RUN: %target-build-swift   \
// RUN:     %t/Executable.o   \
// RUN:     -lLibrary         \
// RUN:     -L %t             \
// RUN:     %target-rpath(%t) \
// RUN:     -o %t/main

// RUN: %target-codesign %t/%target-library-name(Library)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(Library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// TODO: CoroutineAccessors: Enable on WASM.
// UNSUPPORTED: wasm
// UNSUPPORTED: OS=wasi
// UNSUPPORTED: CPU=wasm32

// REQUIRES: swift_feature_CoroutineAccessors

//--- Library.swift

// TODO: CoroutineAccessors: Change to X.Y
@available(SwiftStdlib 9999, *)
public protocol ResilientWrapping {
  associatedtype Wrapped
  var wrapped: Wrapped { read set }
  var wrapped2: Wrapped { read set }
}
@available(SwiftStdlib 9999, *)
extension ResilientWrapping {
  public var wrapped2: Wrapped {
    read {
      yield wrapped
    }
    modify {
      yield &wrapped
    }
  }
}

@available(SwiftStdlib 9999, *)
public struct ResilientBoxtional<T> : ResilientWrapping {
  var storage: T?
  public init(_ t: T?) {
    self.storage = t
  }
  public typealias Wrapped = T?

  public var wrapped : T? {
    read {
      yield storage
    }
    modify {
      yield &storage
    }
  }
}

@available(SwiftStdlib 9999, *)
open class ResilientWrappingClass<Wrapped> {
  public init() {}
  open var wrapped: Wrapped {
    read {
      fatalError()
    }
    modify {
      fatalError()
    }
  }
}

@available(SwiftStdlib 9999, *)
open class ResilientWrappingSubclass<X : ResilientWrapping> : ResilientWrappingClass<X.Wrapped> {
  public init(_ impl: X) { self.impl = impl }
  var impl: X
  open override var wrapped: X.Wrapped {
    read {
      yield impl.wrapped
    }
    modify {
      yield &impl.wrapped
    }
  }
}

//--- Downstream.swift

import Library

@available(SwiftStdlib 9999, *)
struct MaybePtrBox<T> {
  private var ptr: UnsafeMutablePointer<T>

  static func sentinel() -> UnsafeMutablePointer<T> {
    .init(bitPattern: 0xffff)!
  }

  init() {
    ptr = .init(MaybePtrBox.sentinel())
  }
  init(_ t: T) {
    self.init()
    set(t)
  }
  func isValid() -> Bool {
    ptr != MaybePtrBox.sentinel()
  }
  mutating func set(_ t: T?) {
    switch (isValid(), t) {
    case (true, .some(let t)):
      ptr.pointee = t
    case (true, .none):
      ptr.deallocate()
      ptr = MaybePtrBox.sentinel()
    case (false, .some(let t)):
      ptr = .allocate(capacity: 1)
      ptr.initialize(to: t)
    case (false, .none):
      break
    }
  }
  var value : T? {
    // Analogous to the implementation of Dictionary's subscript.
    read {
      let val: T?
      if isValid() {
        val = ptr.pointee
      } else {
        val = .none
      }
      yield val
    }
    modify {
      var val: T?
      if isValid() {
        val = ptr.pointee
      } else {
        val = .none
      }
      yield &val
      set(val)
    }
  }
}

@available(SwiftStdlib 9999, *)
struct Boxtional<T> : ResilientWrapping {
  var storage: T?
  init(_ t: T?) {
    self.storage = t
  }
  typealias Wrapped = T?

  var wrapped : T? {
    read {
      yield storage
    }
    modify {
      yield &storage
    }
  }
}

@available(SwiftStdlib 9999, *)
class NonresilientResilientWrappingSubclass<X : ResilientWrapping> : ResilientWrappingClass<X.Wrapped> {
  init(_ impl: X) { 
    self.impl = impl
    super.init()
  }
  var impl: X
  override var wrapped: X.Wrapped {
    read {
      yield impl.wrapped
    }
    modify {
      yield &impl.wrapped
    }
  }
}

@available(SwiftStdlib 9999, *)
protocol AsyncMutatable {
  mutating func mutate() async
}

@available(SwiftStdlib 9999, *)
struct Stringg : AsyncMutatable {
  var value: String
  mutating func mutate() async {
    value += value
  }
}

@available(SwiftStdlib 9999, *)
protocol Mutatable {
  mutating func mutate()
}

@available(SwiftStdlib 9999, *)
struct Stringgg : Mutatable {
  var value: String
  mutating func mutate() {
    value += value
  }
}

@available(SwiftStdlib 9999, *)
protocol Wrapping {
  associatedtype Wrapped
  var wrapped: Wrapped { read set }
}

@available(SwiftStdlib 9999, *)
extension MaybePtrBox : Wrapping {
  typealias Wrapped = T?
  var wrapped: T? {
    read {
      yield value
    }
    modify {
      yield &value
    }
  }
}
@available(SwiftStdlib 9999, *)
extension MaybePtrBox : ResilientWrapping {}

@available(SwiftStdlib 9999, *)
class WrappingClass<Wrapped> {
  var wrapped: Wrapped {
    read {
      fatalError()
    }
    modify {
      fatalError()
    }
  }
}

@available(SwiftStdlib 9999, *)
class WrappingSubclass<X : Wrapping> : WrappingClass<X.Wrapped> {
  init(_ impl: X) { self.impl = impl }
  var impl: X
  override var wrapped: X.Wrapped {
    read {
      yield impl.wrapped
    }
    modify {
      yield &impl.wrapped
    }
  }
}

@available(SwiftStdlib 9999, *)
extension Optional : Mutatable where Wrapped : Mutatable {
  mutating func mutate() {
    switch (self) {
    case .none:
      break
    case .some(var value):
      value.mutate()
      self = .some(value)
    }
  }
}

@available(SwiftStdlib 9999, *)
@main
struct M {
  static func sync_mutate<T : Mutatable>(_ t: inout T?) {
    var b = MaybePtrBox<T>()
    b.set(t)
    b.value?.mutate()
    t = b.value
  }
  static func sync_main() {
    var v1 = Optional<Stringgg>.none
    // CHECK: nil
    print(v1)
    sync_mutate(&v1)
    // CHECK: nil
    print(v1)
    var v2 = Optional.some(Stringgg(value: "hi"))
    // CHECK: "hi"
    print(v2)
    sync_mutate(&v2)
    // CHECK: "hihi"
    print(v2)
  }
  static func mutate<T : AsyncMutatable>(_ t: inout T?) async {
    var b = MaybePtrBox<T>()
    b.set(t)
    await b.value?.mutate()
    t = b.value
  }
  static func async_main() async {
    var v1 = Optional<Stringg>.none
    // CHECK: nil
    print(v1)
    await mutate(&v1)
    // CHECK: nil
    print(v1)
    var v2 = Optional.some(Stringg(value: "hi"))
    // CHECK: "hi"
    print(v2)
    await mutate(&v2)
    // CHECK: "hihi"
    print(v2)
  }
  static func mutateWrapped<T : Wrapping>(_ t: inout T) where T.Wrapped : Mutatable {
    t.wrapped.mutate()
  }
  static func proto_main() {
    var v1 = MaybePtrBox(Optional<Stringgg>.none)
    // CHECK: nil
    print(v1.value)
    mutateWrapped(&v1)
    // CHECK: nil
    print(v1.value)

    var v2 = MaybePtrBox(Stringgg(value: "hi"))
    // CHECK: "hi"
    print(v2.value)
    mutateWrapped(&v2)
    // CHECK: "hihi"
    print(v2.value)
  }
  static func mutateWrappedInClass<T : Mutatable>(_ t: WrappingClass<T>) {
    t.wrapped.mutate()
  }
  static func class_main() {
    let v1 = MaybePtrBox(Optional<Stringgg>.none)
    // CHECK: nil
    print(v1.value)
    mutateWrappedInClass(WrappingSubclass(v1))
    // CHECK: nil
    print(v1.value)
    let v2 = MaybePtrBox(Stringgg(value: "hi"))
    // CHECK: "hi"
    print(v2.value)
    mutateWrappedInClass(WrappingSubclass(v2))
    // CHECK: "hihi"
    print(v2.value)
  }
  static func mutateResilientWrapped<T : ResilientWrapping>(_ t: inout T) where T.Wrapped : Mutatable {
    t.wrapped.mutate()
  }
  static func resilient_proto_main() {
    var v1 = ResilientBoxtional(Optional<Stringgg>.none)
    // CHECK: nil
    print(v1.wrapped)
    mutateResilientWrapped(&v1)
    // CHECK: nil
    print(v1.wrapped)

    var v2 = ResilientBoxtional(Stringgg(value: "hi"))
    // CHECK: "hi"
    print(v2.wrapped)
    mutateResilientWrapped(&v2)
    // CHECK: "hihi"
    print(v2.wrapped)
  }
  static func mutateResilientWrapped2<T : ResilientWrapping>(_ t: inout T) where T.Wrapped : Mutatable {
    t.wrapped2.mutate()
  }
  static func resilient_proto_default_main() {
    var v1 = Boxtional(Optional<Stringgg>.none)
    // CHECK: nil
    print(v1.wrapped)
    mutateResilientWrapped(&v1)
    // CHECK: nil
    print(v1.wrapped)

    var v2 = Boxtional(Stringgg(value: "hi"))
    // CHECK: "hi"
    print(v2.wrapped)
    mutateResilientWrapped(&v2)
    // CHECK: "hihi"
    print(v2.wrapped)
  }
  static func mutateWrappedInResilientClass<T : Mutatable>(_ t: ResilientWrappingClass<T>) {
    t.wrapped.mutate()
  }
  static func resilient_class_main() {
    let v1 = MaybePtrBox(Optional<Stringgg>.none)
    // CHECK: nil
    print(v1.wrapped)
    mutateWrappedInResilientClass(ResilientWrappingSubclass(v1))
    // CHECK: nil
    print(v1.wrapped)
    let v2 = MaybePtrBox(Stringgg(value: "hi"))
    // CHECK: "hi"
    print(v2.wrapped)
    mutateWrappedInResilientClass(ResilientWrappingSubclass(v2))
    // CHECK: "hihi"
    print(v2.wrapped)
  }
  static func resilient_subclass_main() {
    let v1 = MaybePtrBox(Optional<Stringgg>.none)
    // CHECK: nil
    print(v1.wrapped)
    mutateWrappedInResilientClass(NonresilientResilientWrappingSubclass(v1))
    // CHECK: nil
    print(v1.wrapped)
    let v2 = MaybePtrBox(Stringgg(value: "hi"))
    // CHECK: "hi"
    print(v2.wrapped)
    mutateWrappedInResilientClass(NonresilientResilientWrappingSubclass(v2))
    // CHECK: "hihi"
    print(v2.wrapped)
  }
  static func main() async {
    sync_main()
    await async_main()
    proto_main()
    class_main()
    resilient_proto_main()
    resilient_proto_default_main()
    resilient_class_main()
    resilient_subclass_main()
  }
}
