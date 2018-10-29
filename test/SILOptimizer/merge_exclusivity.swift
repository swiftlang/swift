// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -emit-sil  -primary-file %s | %FileCheck %s --check-prefix=TESTSIL
// REQUIRES: optimized_stdlib,asserts
// REQUIRES: PTRSIZE=64

public var check: UInt64 = 0

@inline(never)
func sum(_ x: UInt64, _ y: UInt64) -> UInt64 {
  return x &+ y
}

// TESTSIL-LABEL: sil [noinline] @$s17merge_exclusivity10MergeTest1yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$s17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL: bb5
// TESTSIL: [[B2:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: load [[B2]]
// TESTSIL: store {{.*}} to [[B2]]
// TESTSIL: end_access [[B2]]
// TESTSIL: bb6
// TESTSIL: [[B3:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: load [[B3]]
// TESTSIL: store {{.*}} to [[B3]]
// TESTSIL: end_access [[B3]]
// TESTSIL: bb7
// TESTSIL: [[B4:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: load [[B4]]
// TESTSIL: store {{.*}} to [[B4]]
// TESTSIL: end_access [[B4]]
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$s17merge_exclusivity10MergeTest1yySiF'
@inline(never)
public func MergeTest1(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
      if (e == 0) {
        check = sum(check, UInt64(1))
      }
      else {
        check = sum(check, UInt64(2))
      }
    }
  }
}

// TESTSIL-LABEL: sil [noinline] @$s17merge_exclusivity10MergeTest2yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$s17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL: bb6
// TESTSIL: [[B2:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: load [[B2]]
// TESTSIL: store {{.*}} to [[B2]]
// TESTSIL: end_access [[B2]]
// TESTSIL: bb7
// TESTSIL: [[B3:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: load [[B3]]
// TESTSIL: store {{.*}} to [[B3]]
// TESTSIL: end_access [[B3]]
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$s17merge_exclusivity10MergeTest2yySiF'
@inline(never)
public func MergeTest2(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      if (e == 0) {
        check = sum(check, UInt64(1))
      }
      else {
        check = sum(check, UInt64(2))
      }
    }
  }
}

// TESTSIL-LABEL: sil [noinline] @$s17merge_exclusivity10MergeTest3yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$s17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$s17merge_exclusivity10MergeTest3yySiF'
@inline(never)
public func MergeTest3(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
    }
  }
}

// TESTSIL-LABEL: sil [noinline] @$s17merge_exclusivity10MergeTest4yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$s17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL: bb7
// TESTSIL: [[B2:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: load [[B2]]
// TESTSIL: store {{.*}} to [[B2]]
// TESTSIL: end_access [[B2]]
// TESTSIL: bb8
// TESTSIL: [[B3:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: load [[B3]]
// TESTSIL: store {{.*}} to [[B3]]
// TESTSIL: end_access [[B3]]
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$s17merge_exclusivity10MergeTest4yySiF'
@inline(never)
public func MergeTest4(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      if (e == 0) {
        check = sum(check, UInt64(1))
      }
      check = sum(check, UInt64(e))
    }
  }
}

// TESTSIL-LABEL: sil [noinline] @$s17merge_exclusivity10MergeTest5yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$s17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL: bb6
// TESTSIL: [[B2:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: load [[B2]]
// TESTSIL: store {{.*}} to [[B2]]
// TESTSIL: end_access [[B2]]
// TESTSIL: bb7
// TESTSIL: [[B3:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: load [[B3]]
// TESTSIL: store {{.*}} to [[B3]]
// TESTSIL: end_access [[B3]]
// TESTSIL: bb8
// TESTSIL: [[B4:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: load [[B4]]
// TESTSIL: store {{.*}} to [[B4]]
// TESTSIL: end_access [[B4]]
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$s17merge_exclusivity10MergeTest5yySiF'
@inline(never)
public func MergeTest5(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      if (e == 0) {
        check = sum(check, UInt64(1))
      }
      else {
        check = sum(check, UInt64(2))
      }
      check = sum(check, UInt64(e))
    }
  }
}

// TESTSIL-LABEL: sil [noinline] @$s17merge_exclusivity10MergeTest6yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$s17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$s17merge_exclusivity10MergeTest6yySiF'
@inline(never)
public func MergeTest6(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
      for _ in range {
        check = sum(check, UInt64(e))
      }
      check = sum(check, UInt64(e))
    }
  }
}

@inline(never)
public func foo() {
}

// TESTSIL-LABEL: sil [noinline] @$s17merge_exclusivity10MergeTest7yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$s17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$s17merge_exclusivity10MergeTest7yySiF'
@inline(never)
public func MergeTest7(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
      for _ in range {
        foo()
      }
      check = sum(check, UInt64(e))
    }
  }
}

// TESTSIL-LABEL: sil [noinline] @$s17merge_exclusivity10MergeTest8yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$s17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$s17merge_exclusivity10MergeTest8yySiF'
@inline(never)
public func MergeTest8(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
      for _ in range {
        foo()
      }
      check = sum(check, UInt64(e))
    }
  }
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
      for _ in range {
        foo()
      }
      check = sum(check, UInt64(e))
    }
  }
}

// Large, test that tests the interaction between access merging,
// and the rest of the access optimizations.

public protocol WriteProt {
    func writeTo(_ stream: StreamClass)
}

public final class StreamClass {
    private var buffer: [UInt8]
    
    public init() {
        self.buffer = []
    }

    public func write(_ byte: UInt8) {
        buffer.append(byte)
    }

    public func write(_ value: WriteProt) {
        value.writeTo(self)
    }

    public func writeEscaped(_ string: String) {
        writeEscaped(string: string.utf8)
    }
    
    public func writeEscaped<T: Collection>(
        string sequence: T
    ) where T.Iterator.Element == UInt8 {
        for character in sequence {
            buffer.append(character)
            buffer.append(character)
        }
    }
}

public func toStream(_ stream: StreamClass, _ value: WriteProt) -> StreamClass {
    stream.write(value)
    return stream
}

extension UInt8: WriteProt {
    public func writeTo(_ stream: StreamClass) {
        stream.write(self)
    }
}

public func asWriteProt(_ string: String) -> WriteProt {
        return EscapedString(value: string)
}

private struct EscapedString: WriteProt {
    let value: String
        
    func writeTo(_ stream: StreamClass) {
        _ = toStream(stream, UInt8(ascii: "a"))
        stream.writeEscaped(value)
        _ = toStream(stream, UInt8(ascii: "a"))
    }
}

public func asWriteProt<T>(_ items: [T], transform: @escaping (T) -> String) -> WriteProt {
    return EscapedTransforme(items: items, transform: transform)
}

private struct EscapedTransforme<T>: WriteProt {
    let items: [T]
    let transform: (T) -> String

    func writeTo(_ stream: StreamClass) {
        for (i, item) in items.enumerated() {
            if i != 0 { _ = toStream(stream, asWriteProt(transform(item))) }
            _ = toStream(stream, asWriteProt(transform(item)))
        }
    }
}

// TESTSIL-LABEL: sil [noinline] @$s17merge_exclusivity14run_MergeTest9yySiF : $@convention(thin)
// TESTSIL: [[REFADDR:%.*]] = ref_element_addr {{.*}} : $StreamClass, #StreamClass.buffer
// TESTSIL-NEXT: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[REFADDR]]
// TESTSIL: end_access [[B1]]
// TESTSIL: [[BCONF:%.*]] = begin_access [modify] [dynamic] [[REFADDR]]
// TESTSIL: apply {{.*}} : $@convention(method) (Int, @inout Array<UInt8>) -> ()
// TESTSIL: end_access [[BCONF]]
// TESTSIL: [[BCONF:%.*]] = begin_access [modify] [dynamic] [[REFADDR]]
// TESTSIL: apply {{.*}} : $@convention(method) (Int, @inout Array<UInt8>) -> ()
// TESTSIL: end_access [[BCONF]]
// TESTSIL: [[BCONF:%.*]] = begin_access [modify] [dynamic] [[REFADDR]]
// TESTSIL: apply {{.*}} : $@convention(method) (Int, @inout Array<UInt8>) -> ()
// TESTSIL: end_access [[BCONF]]
// TESTSIL-LABEL: } // end sil function '$s17merge_exclusivity14run_MergeTest9yySiF'
@inline(never)
public func run_MergeTest9(_ N: Int) {
  struct Thing {
    var value: String
    init(_ value: String) { self.value = value }
  }
  let listOfStrings: [String] = (0..<10).map { "This is the number: \($0)!\n" }
  let listOfThings: [Thing] = listOfStrings.map(Thing.init)
  for _ in 1...N {
    let stream = StreamClass()
      _ = toStream(stream, asWriteProt(listOfThings, transform: { $0.value }))
  }
}
