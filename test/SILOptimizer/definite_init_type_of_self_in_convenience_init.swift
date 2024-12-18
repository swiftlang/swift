// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -swift-version 4 -verify %s | %FileCheck %s
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -swift-version 5 -verify %s | %FileCheck %s

// Integration test to ensure that `type(of: self)` keeps working in
// class convenience initializers, even though they are now implemented as
// allocating entry points.

class C {
  required init() { }
  required init(throwingDesignated: ()) throws {}

  convenience init(normal: ()) {
    _ = (type(of: self), type(of: self))
    self.init()
    _ = (type(of: self), type(of: self))
  }

  convenience init(throwing: ()) throws {
    do {
      _ = (type(of: self), type(of: self))
      try self.init(throwingDesignated: ())
      _ = (type(of: self), type(of: self))
    } catch {
      _ = (type(of: self), type(of: self))
      throw error
    }
    _ = (type(of: self), type(of: self))
  }

  convenience init?(optional: Bool) {
    _ = (type(of: self), type(of: self))
    if optional {
      _ = (type(of: self), type(of: self))
      self.init()
    } else {
      _ = (type(of: self), type(of: self))
      return nil
    }
    _ = (type(of: self), type(of: self))
  }

  convenience init(closureCapture: ()) {
    let t = type(of: self)
    let fn = { t.init() }
    _ = fn()
    self.init()
  }
}

protocol P {
  static var n: Int { get }
  init(_: Int)
}

extension P {
  // FIXME: SILGen inserts an unnecessary copy when 'self' is
  // address-only.
  /* init(selfInit: ()) {
    self.init(type(of: self).n)
  }

  init(selfAssign: ()) {
    self = type(of: self).init(0)
  } */
}

protocol PA : AnyObject {
  static var n: Int { get }
  init(_: Int)
}

extension PA {
  init(selfInit: ()) {
    // This is OK; we can get the type of 'self' from the self metatype
    // argument.
    self.init(type(of: self).n)
  }

  // FIXME: Not yet supported, but should be
  /* init(selfAssign: ()) {
    self = type(of: self).init(0)
  } */
}

class CC {
  class var n: Int { 0 }
  required init(_: Int) {}
}

protocol PC : CC {}

extension PC {
  // CHECK-LABEL: sil hidden @$s042definite_init_type_of_self_in_convenience_B02PCPAAE0E4Initxyt_tcfC : $@convention(method) <Self where Self : PC> (@thick Self.Type) -> @owned Self {
  init(selfInit: ()) {
    // This is OK; we can get the type of 'self' from the self metatype
    // argument.
    self.init(type(of: self).n)

    // CHECK: [[SELF:%.*]] = upcast %0 : $@thick Self.Type to $@thick CC.Type
    // CHECK: [[INIT:%.*]] = class_method [[SELF]] : $@thick CC.Type, #CC.init!allocator : (CC.Type) -> (Int) -> CC, $@convention(method) (Int, @thick CC.Type) -> @owned CC
    // CHECK: apply [[INIT]]({{.*}}, [[SELF]]) : $@convention(method) (Int, @thick CC.Type) -> @owned CC
  }

  // FIXME: Not yet supported, but should be
  /* init(selfAssign: ()) {
    self = type(of: self).init(0)
  } */
}
