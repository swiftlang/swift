// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -wmo -enable-default-cmo -parse-as-library %t/Lib.swift -emit-module-path=%t/Lib.swiftmodule -module-name=Lib -enable-testing -I%t

/// TEST: Deserializing Lib module should not cause an assert fail.
// RUN: %target-swift-frontend -emit-sil -sil-verify-all %t/Client.swift -I%t

// REQUIRES: swift_in_compiler
// REQUIRES: asserts

//--- Client.swift

@testable import Lib


//--- Lib.swift

public class PubKlass {
  var field = InternalStruct()
  /// This function should not be serialized as it returns an opaque type that isn't cast to a concrete nominal type.
  func run(from arg: PubStruct) -> some Sequence<InternalStruct.Klass> {
    field.run(from: arg)
  }
}

public struct PubStruct: Hashable { }

struct InternalStruct: Hashable {
  class Klass: Hashable {
    let data: PubStruct
    init(_ arg: PubStruct) {
      self.data = arg
    }
    func hash(into hasher: inout Hasher) {
        hasher.combine(data)
    }
    static func == (lhs: Klass, rhs: Klass) -> Bool {
        return lhs.data == rhs.data
    }
  }

  var entities: [PubStruct: Klass] = [:]

  func run(from arg: PubStruct) -> some Sequence<Klass> {
    IteratorSequence(EntityIterator(from: arg))
      .lazy.map { entities[$0]! }
  }
}


private struct EntityIterator<T: Hashable>: IteratorProtocol {
  private var list: [T]
  init(from start: T) {
    self.list = [start]
  }
  private mutating func pop() -> T? {
    return nil
  }
  mutating func next() -> T? {
    return nil
  }
}


