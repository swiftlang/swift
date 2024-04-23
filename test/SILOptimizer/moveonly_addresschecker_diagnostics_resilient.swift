// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                                      \
// RUN:     %t/Library.swift                                        \
// RUN:     -emit-module                                            \
// RUN:     -enable-library-evolution                               \
// RUN:     -module-name Library                                    \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-swift-frontend                                      \
// RUN:     %t/Client.swift                                         \
// RUN:     -emit-sil -verify                                       \
// RUN:     -debug-diagnostic-names                                 \
// RUN:     -sil-verify-all                                         \
// RUN:     -I %t


//--- Library.swift

public struct Ur : ~Copyable {
  deinit {}
}
public func consume(_ ur: consuming Ur) {}
public func borrow(_ ur: borrowing Ur) {}

@frozen
public struct AggFrozen : ~Copyable {
  public var field1: Ur
  public var field2: Ur
}
public func consume(_ a: consuming AggFrozen) {}
public func borrow(_ ur: borrowing AggFrozen) {}

@frozen
public struct AggFrozenDeiniting : ~Copyable {
  public var field1: Ur
  public var field2: Ur
  deinit {}
}
public func consume(_ a: consuming AggFrozenDeiniting) {}
public func borrow(_ ur: borrowing AggFrozenDeiniting) {}

public struct AggResilient : ~Copyable {
  public var field1: Ur
  public var field2: Ur
}
public func consume(_ a: consuming AggResilient) {}
public func borrow(_ ur: borrowing AggResilient) {}

public struct AggResilientDeiniting : ~Copyable {
  public var field1: Ur
  public var field2: Ur
  deinit {}
}
public func consume(_ a: consuming AggResilientDeiniting) {}
public func borrow(_ ur: borrowing AggResilientDeiniting) {}

//--- Client.swift

import Library

struct AggLocalDeiniting : ~Copyable {
  var field1: Ur
  var field2: Ur
  deinit {}
}

struct AggLocal : ~Copyable {
  var field1: Ur
  var field2: Ur
}

func consumeField1_AggFrozen(_ a: consuming AggFrozen) {
  consume(a.field1)
}

func consumeField1_AggFrozenDeiniting(_ a: consuming AggFrozenDeiniting) {
  consume(a.field1) // expected-error{{cannot partially consume 'a' when it has a deinitializer}}
}

func consumeField1_AggResilient(_ a: consuming AggResilient) {
  consume(a.field1) // expected-error{{field 'a.field1' was consumed but not reinitialized; the field must be reinitialized during the access}}
                    // expected-note@-1{{consumed here}}
}

func consumeField1_AggResilientDeiniting(_ a: consuming AggResilientDeiniting) {
  consume(a.field1) // expected-error{{field 'a.field1' was consumed but not reinitialized; the field must be reinitialized during the access}}
                    // expected-note@-1{{consumed here}}
}
