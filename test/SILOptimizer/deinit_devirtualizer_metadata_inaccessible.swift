// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -target %target-swift-6.2-abi-triple -emit-module -O -wmo -parse-as-library -module-name Lib -emit-module-path %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-swift-frontend -target %target-swift-6.2-abi-triple -emit-sil -O -wmo -module-name main -I %t -sil-verify-all %t/Main.swift | %FileCheck %s
// RUN: %target-swift-frontend -target %target-swift-6.2-abi-triple -emit-ir -O -wmo -module-name main -I %t %t/Main.swift | %FileCheck --check-prefix=IR %s
// RUN: %target-swift-frontend -target %target-swift-6.2-abi-triple -emit-ir -Onone -wmo -module-name main -I %t %t/Main.swift | %FileCheck --check-prefix=IR %s

// Check that destroys of non-copyable types are not decomposed into destroys of fields which
// have a deinit and whose type metadata is not accessible in the client module, e.g. an internal
// type from another module. Calling the deinit would require the field type's metadata.
// This must also hold for serialized (@inlinable) functions in the defining module, because
// they are inlined into clients.
// https://github.com/swiftlang/swift/issues/94452

// IR-NOT: $s3Lib5InnerV
// IR-NOT: $s3Lib7WrapperV
// IR-NOT: $s3Lib1EO
// IR-NOT: $s3Lib5PairEO

//--- Lib.swift

struct Inner: ~Copyable {
  let p: UnsafeMutablePointer<Int>
  init() { p = .allocate(capacity: 1) }
  deinit { p.deallocate() }
}

// Internal without deinit, but containing a field with a deinit.
struct Wrapper: ~Copyable {
  var inner = Inner()
  var o: AnyObject? = nil
}

enum E: ~Copyable {
  case a(Inner)
  case b
}

public struct Manager: ~Copyable {
  var inner: Inner
  public init() { inner = Inner() }
}

public struct Manager2: ~Copyable {
  var w = Wrapper()
  var o: AnyObject? = nil
  public init() {}
}

public struct Manager3: ~Copyable {
  var e: E = .a(Inner())
  var o: AnyObject? = nil
  public init() {}
}

// The payload of `pair` is a tuple.
enum PairE: ~Copyable {
  case empty
  case pair(Inner, Int)
}

public struct PairManager: ~Copyable {
  var e: PairE = .pair(Inner(), 0)
  public init() {}
}

public struct ArrayManager: ~Copyable {
  var arr: InlineArray<2, Inner> = InlineArray { _ in Inner() }
  public init() {}
}

@inlinable
public func consumeInLib(_ m: consuming Manager) {}

@inlinable
public func consumePairInLib(_ m: consuming PairManager) {}

@inlinable
public func consumeArrayInLib(_ m: consuming ArrayManager) {}

//--- Main.swift

import Lib

// CHECK-LABEL: sil [noinline] @$s4main14consumeManageryy3Lib0C0VnF :
// CHECK-NOT:     struct_extract
// CHECK:         release_value %0
// CHECK:       } // end sil function '$s4main14consumeManageryy3Lib0C0VnF'
@inline(never)
public func consumeManager(_ m: consuming Manager) {}

// CHECK-LABEL: sil [noinline] @$s4main15consumeManager2yy3Lib0C0VnF :
// CHECK-NOT:     struct_extract
// CHECK:         release_value %0
// CHECK:       } // end sil function '$s4main15consumeManager2yy3Lib0C0VnF'
@inline(never)
public func consumeManager2(_ m: consuming Manager2) {}

// CHECK-LABEL: sil [noinline] @$s4main15consumeManager3yy3Lib0C0VnF :
// CHECK-NOT:     struct_extract
// CHECK:         release_value %0
// CHECK:       } // end sil function '$s4main15consumeManager3yy3Lib0C0VnF'
@inline(never)
public func consumeManager3(_ m: consuming Manager3) {}

// The serialized body of `consumeInLib` must not contain a destroy of `Inner`.
// CHECK-LABEL: sil @$s4main13testInlinableyyF :
// CHECK-NOT:     struct_extract
// CHECK:         release_value
// CHECK:       } // end sil function '$s4main13testInlinableyyF'
public func testInlinable() {
  consumeInLib(Manager())
}

// CHECK-LABEL: sil @$s4main17testInlinablePairyyF :
// CHECK-NOT:     switch_enum
// CHECK-NOT:     destructure_tuple
// CHECK:         release_value
// CHECK:       } // end sil function '$s4main17testInlinablePairyyF'
public func testInlinablePair() {
  consumePairInLib(PairManager())
}

// CHECK-LABEL: sil @$s4main18testInlinableArrayyyF :
// CHECK-NOT:     struct_extract
// CHECK-NOT:     destroy_addr
// CHECK:         release_value
// CHECK:       } // end sil function '$s4main18testInlinableArrayyyF'
public func testInlinableArray() {
  consumeArrayInLib(ArrayManager())
}

// CHECK-LABEL: sil hidden @$s4main6HolderCfD :
// CHECK-NOT:     struct_extract
// CHECK-NOT:     struct_element_addr
// CHECK:       } // end sil function '$s4main6HolderCfD'
final class Holder {
  var m = Manager()
  var m2 = Manager2()
  var m3 = Manager3()
}
