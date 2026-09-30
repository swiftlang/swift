// RUN: %target-swift-emit-silgen -module-name resolve -enable-lifetime-resolution %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -module-name resolve -enable-lifetime-resolution -enable-sil-opaque-values %s | %FileCheck %s --check-prefix=OPAQUE

// Verifies the raw SILGen emission of the `consume` operator when
// `-enable-lifetime-resolution` is enabled. In this mode `consume` may be
// applied to more kinds of storage (copyable let/var bindings, tuple elements,
// and stored fields of copyable and generic structs) than without the flag.

class C {}

struct NC: ~Copyable {
  let c = C()
}

struct CopyableType {
  var a: C = C()
  var b: C = C()
  var c: C = C()
}

struct GenericType<T> {
  var a: T
  var b: T
  var c: T
}

func use(_ c: C) {}
func useNC(_ c: borrowing NC) {}
func useGen<T>(_ t: T) {}

// A loadable `let` is an SSA value, so the consume takes the object path: a
// `move_value [allows_diagnostics]`, with no access scope anywhere.
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve15testLoadableLetyyF : $@convention(thin) () -> () {
// CHECK:         move_value [lexical] [var_decl]
// CHECK:         [[BORROW:%.*]] = begin_borrow
// CHECK-NEXT:    [[COPY:%.*]] = copy_value [[BORROW]]
// CHECK-NEXT:    move_value [allows_diagnostics] [[COPY]]
// CHECK-NOT:     begin_access
// CHECK:       } // end sil function '$s7resolve15testLoadableLetyyF'
func testLoadableLet() {
  let x = C()
  _ = consume x
  use(x)
}

// A loadable `var` lives in a box: consume it with a `[deinit]` access and a
// direct `load [take]` (no `mark_unresolved_move_addr`).
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve15testLoadableVaryyF : $@convention(thin) () -> () {
// CHECK:         [[ACCESS:%.*]] = begin_access [deinit] [unknown]
// CHECK-NEXT:    [[VALUE:%.*]] = load [take] [[ACCESS]]
// CHECK-NEXT:    end_access [[ACCESS]]
// CHECK:       } // end sil function '$s7resolve15testLoadableVaryyF'
func testLoadableVar() {
  var x = C()
  _ = consume x
  use(x)
}

// A noncopyable `var` is the same as the loadable var, plus the
// mark_unresolved_non_copyable_value marker inside the `[deinit]` access.
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve18testNoncopyableVaryyF : $@convention(thin) () -> () {
// CHECK:         [[ACCESS:%.*]] = begin_access [deinit] [unknown]
// CHECK-NEXT:    [[MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[ACCESS]]
// CHECK-NEXT:    load [take] [[MARK]]
// CHECK-NEXT:    end_access [[ACCESS]]
// CHECK:       } // end sil function '$s7resolve18testNoncopyableVaryyF'
func testNoncopyableVar() {
  var x = NC()
  _ = consume x
  useNC(x)
}

// An address-only `var` takes into a temporary via `copy_addr [take]` out of
// the `[deinit]` access.
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve18testAddressOnlyVaryyxlF : $@convention(thin) <T> (@in_guaranteed T) -> () {
// CHECK:         [[ACCESS:%.*]] = begin_access [deinit] [unknown]
// CHECK-NEXT:    [[TMP:%.*]] = alloc_stack $T
// CHECK-NEXT:    copy_addr [take] [[ACCESS]] to [init] [[TMP]]
// CHECK-NEXT:    end_access [[ACCESS]]
// CHECK:       } // end sil function '$s7resolve18testAddressOnlyVaryyxlF'
//
// Under `-enable-sil-opaque-values` an address-only type is an SSA value, so the
// consume takes it directly with `load [take]`.
//
// OPAQUE-LABEL: sil hidden [ossa] [opaque] @$s7resolve18testAddressOnlyVaryyxlF : $@convention(thin) <T> (@in_guaranteed T) -> () {
// OPAQUE:         [[ACCESS:%.*]] = begin_access [deinit] [unknown]
// OPAQUE-NEXT:    load [take] [[ACCESS]]
// OPAQUE-NEXT:    end_access [[ACCESS]]
// OPAQUE:       } // end sil function '$s7resolve18testAddressOnlyVaryyxlF'
func testAddressOnlyVar<T>(_ t: T) {
  var x = t
  _ = consume x
  useGen(x)
}

// Consuming a tuple element projects with `tuple_element_addr` inside the
// `[deinit]` access, then takes.
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve16testTupleElementyyF : $@convention(thin) () -> () {
// CHECK:         [[ACCESS:%.*]] = begin_access [deinit] [unknown]
// CHECK-NEXT:    [[ELT:%.*]] = tuple_element_addr [[ACCESS]], 1
// CHECK-NEXT:    load [take] [[ELT]]
// CHECK-NEXT:    end_access [[ACCESS]]
// CHECK:       } // end sil function '$s7resolve16testTupleElementyyF'
func testTupleElement() {
  var x = (C(), C())
  _ = consume x.1
  use(x.1)
}

// Consuming a stored field of a copyable struct projects with
// `struct_element_addr` inside the `[deinit]` access, then takes.
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve15testStructFieldyyF : $@convention(thin) () -> () {
// CHECK:         [[ACCESS:%.*]] = begin_access [deinit] [unknown]
// CHECK-NEXT:    [[ELT:%.*]] = struct_element_addr [[ACCESS]], #CopyableType.a
// CHECK-NEXT:    load [take] [[ELT]]
// CHECK-NEXT:    end_access [[ACCESS]]
// CHECK:       } // end sil function '$s7resolve15testStructFieldyyF'
func testStructField() {
  var ct = CopyableType()
  _ = consume ct.a
  use(ct.a)
}

// The address-only counterpart of the struct-field case: project the field,
// then `copy_addr [take]` into a temporary.
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve22testGenericStructFieldyyxlF : $@convention(thin) <T> (@in_guaranteed T) -> () {
// CHECK:         [[ACCESS:%.*]] = begin_access [deinit] [unknown]
// CHECK-NEXT:    [[ELT:%.*]] = struct_element_addr [[ACCESS]], #GenericType.a
// CHECK-NEXT:    [[TMP:%.*]] = alloc_stack $T
// CHECK-NEXT:    copy_addr [take] [[ELT]] to [init] [[TMP]]
// CHECK-NEXT:    end_access [[ACCESS]]
// CHECK:       } // end sil function '$s7resolve22testGenericStructFieldyyxlF'
//
// As above, under `-enable-sil-opaque-values` the field is taken with
// `load [take]` rather than `copy_addr [take]`.
//
// OPAQUE-LABEL: sil hidden [ossa] [opaque] @$s7resolve22testGenericStructFieldyyxlF : $@convention(thin) <T> (@in_guaranteed T) -> () {
// OPAQUE:         [[ACCESS:%.*]] = begin_access [deinit] [unknown]
// OPAQUE-NEXT:    [[ELT:%.*]] = struct_element_addr [[ACCESS]], #GenericType.a
// OPAQUE-NEXT:    load [take] [[ELT]]
// OPAQUE-NEXT:    end_access [[ACCESS]]
// OPAQUE:       } // end sil function '$s7resolve22testGenericStructFieldyyxlF'
func testGenericStructField<T>(_ t: T) {
  var gt = GenericType(a: t, b: t, c: t)
  _ = consume gt.a
  useGen(gt.a)
}

// Consuming a trivial tuple element still emits the `[deinit]` access; the take
// degenerates to a `load [trivial]`.
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve23testTrivialTupleElementyyF : $@convention(thin) () -> () {
// CHECK:         [[ACCESS:%.*]] = begin_access [deinit] [unknown]
// CHECK-NEXT:    [[ELT:%.*]] = tuple_element_addr [[ACCESS]], 0
// CHECK-NEXT:    load [trivial] [[ELT]]
// CHECK-NEXT:    end_access [[ACCESS]]
// CHECK:       } // end sil function '$s7resolve23testTrivialTupleElementyyF'
func testTrivialTupleElement() {
  var tup = (1, 2)
  _ = consume tup.0
  _ = tup.0 + tup.1
}

struct S {
  var computed: C { C() }
  subscript(i: Int) -> C { C() }
}

func makeC() -> C { C() }

struct Leaf {
  var c: C = C()
}

struct Subscriptable {
  subscript(i: Int) -> Leaf { Leaf() }
}

struct Nested {
  var storedLeaf: Leaf = Leaf()
  var storedSub: Subscriptable = Subscriptable()
  var computedLeaf: Leaf { Leaf() }
  func makeLeaf() -> Leaf { Leaf() }
}

// Consuming a computed property consumes the getter's owned result directly.
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve27testConsumeComputedPropertyyyAA1SVF : $@convention(thin) (S) -> () {
// CHECK:         [[VAL:%.*]] = apply {{%[0-9]+}}(%0) : $@convention(method) (S) -> @owned C
// CHECK-NEXT:    move_value [allows_diagnostics] [[VAL]]
// CHECK:       } // end sil function '$s7resolve27testConsumeComputedPropertyyyAA1SVF'
func testConsumeComputedProperty(_ s: S) {
  _ = consume s.computed
}

// Consuming a function-call result consumes the call's owned result directly.
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve21testConsumeCallResultyyF : $@convention(thin) () -> () {
// CHECK:         [[VAL:%.*]] = apply {{%[0-9]+}}() : $@convention(thin) () -> @owned C
// CHECK-NEXT:    move_value [allows_diagnostics] [[VAL]]
// CHECK:       } // end sil function '$s7resolve21testConsumeCallResultyyF'
func testConsumeCallResult() {
  _ = consume makeC()
}

// Consuming a subscript result consumes the subscript getter's owned result.
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve20testConsumeSubscriptyyAA1SVF : $@convention(thin) (S) -> () {
// CHECK:         [[VAL:%.*]] = apply {{%[0-9]+}}({{%[0-9]+}}, %0) : $@convention(method) (Int, S) -> @owned C
// CHECK-NEXT:    move_value [allows_diagnostics] [[VAL]]
// CHECK:       } // end sil function '$s7resolve20testConsumeSubscriptyyAA1SVF'
func testConsumeSubscript(_ s: S) {
  _ = consume s[0]
}

// A stored field reached through a computed-property base: the base getter
// runs, the field is projected and copied out, and that copy is consumed.
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve31testConsumeChainThroughComputedyyAA6NestedVF : $@convention(thin) (@guaranteed Nested) -> () {
// CHECK:         apply {{%[0-9]+}}(%0) : $@convention(method) (@guaranteed Nested) -> @owned Leaf
// CHECK:         [[FIELD:%.*]] = struct_extract {{%[0-9]+}}, #Leaf.c
// CHECK:         [[COPY:%.*]] = copy_value [[FIELD]]
// CHECK:         move_value [allows_diagnostics] [[COPY]]
// CHECK:       } // end sil function '$s7resolve31testConsumeChainThroughComputedyyAA6NestedVF'
func testConsumeChainThroughComputed(_ a: Nested) {
  _ = consume a.computedLeaf.c
}

// A stored field reached through a subscript base.
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve32testConsumeChainThroughSubscriptyyAA6NestedVF : $@convention(thin) (@guaranteed Nested) -> () {
// CHECK:         struct_extract %0, #Nested.storedSub
// CHECK:         apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (Int, Subscriptable) -> @owned Leaf
// CHECK:         [[FIELD:%.*]] = struct_extract {{%[0-9]+}}, #Leaf.c
// CHECK:         [[COPY:%.*]] = copy_value [[FIELD]]
// CHECK:         move_value [allows_diagnostics] [[COPY]]
// CHECK:       } // end sil function '$s7resolve32testConsumeChainThroughSubscriptyyAA6NestedVF'
func testConsumeChainThroughSubscript(_ a: Nested) {
  _ = consume a.storedSub[0].c
}

// A stored field reached through a call base.
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve27testConsumeChainThroughCallyyAA6NestedVF : $@convention(thin) (@guaranteed Nested) -> () {
// CHECK:         apply {{%[0-9]+}}(%0) : $@convention(method) (@guaranteed Nested) -> @owned Leaf
// CHECK:         [[FIELD:%.*]] = struct_extract {{%[0-9]+}}, #Leaf.c
// CHECK:         [[COPY:%.*]] = copy_value [[FIELD]]
// CHECK:         move_value [allows_diagnostics] [[COPY]]
// CHECK:       } // end sil function '$s7resolve27testConsumeChainThroughCallyyAA6NestedVF'
func testConsumeChainThroughCall(_ a: Nested) {
  _ = consume a.makeLeaf().c
}

// A fully-stored chain is real in-place storage, so -- like the single-field
// cases above -- it takes the storage path: a `[deinit]` access, nested
// `struct_element_addr` projections, then `load [take]` (no `move_value`).
//
// CHECK-LABEL: sil hidden [ossa] @$s7resolve22testConsumeStoredChainyyF : $@convention(thin) () -> () {
// CHECK:         [[ACCESS:%.*]] = begin_access [deinit] [unknown]
// CHECK-NEXT:    [[OUTER:%.*]] = struct_element_addr [[ACCESS]], #Nested.storedLeaf
// CHECK-NEXT:    [[INNER:%.*]] = struct_element_addr [[OUTER]], #Leaf.c
// CHECK-NEXT:    load [take] [[INNER]]
// CHECK-NEXT:    end_access [[ACCESS]]
// CHECK:       } // end sil function '$s7resolve22testConsumeStoredChainyyF'
func testConsumeStoredChain() {
  var a = Nested()
  _ = consume a.storedLeaf.c
}
