// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -module-name test \
// RUN:   -enable-builtin-module \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -enable-experimental-feature NoncopyableGenerics

// REQUIRES: swift_in_compiler

import Builtin

struct BV : ~Escapable {
  let p: UnsafeRawPointer
  let c: Int
  @_unsafeNonescapableResult
  init(_ p: UnsafeRawPointer, _ c: Int) {
    self.p = p
    self.c = c
  }

  public var isEmpty: Bool { c == 0 }
}

struct NE : ~Escapable {
  var bv: BV

  @_unsafeNonescapableResult
  init(_ bv: BV) {
    self.bv = bv
  }
}

struct NC : ~Copyable {
  let p: UnsafeRawPointer
  let c: Int

  func withBufferView<ResultType>(_ body: (BV) throws -> ResultType) rethrows
    -> ResultType {
    try body(BV(p, c))
  }
}

func takeNoescapeInt(_ f: ()->Int) -> Int { f() }

func bv_extract_pointer(_ bv: BV) -> UnsafeRawPointer {
  return bv.p
}

func bv_argument(bv: BV) -> UnsafeRawPointer {
  return bv_extract_pointer(bv)
}

func bv_capture_noescape(bv: BV) ->Int {
  return takeNoescapeInt { bv.c } 
}

func bv_assign_let(_ bv: BV) -> UnsafeRawPointer {
  let local = bv
  return local.p
}

func bv_assign_var(_ bv1: BV, _ bv2: BV, _ z: Bool) -> UnsafeRawPointer {
  var local: BV
  if z {
    local = bv1
  } else {
    local = bv2
  }
  return local.p
}

func bv_assign_field(_ bv: BV) -> UnsafeRawPointer {
  let ne = NE(bv)
  return ne.bv.p
}
