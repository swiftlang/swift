// RUN: %target-typecheck-verify-swift                              \
// RUN:     -disable-availability-checking                          \
// RUN:     -enable-experimental-feature NoImplicitCopy             \
// RUN:     -debug-diagnostic-names

// REQUIRES: swift_feature_NoImplicitCopy

@_silgen_name("get")
func get<T : ~Copyable>(_ t: T.Type = T.self) -> T

class C {}

// =============================================================================
// ====================== NONCOPYABLE NON-GENERIC (BEGIN) ===================={{
// =============================================================================

extension Quad_NC {
  consuming func explicitEveryLeaf() {
    _ = consume p1.u1
    _ = consume p1.u2
    _ = consume p1.c // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p2.u1
    _ = consume p2.u2
    _ = consume p2.c // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
  }

  consuming func explicitSomeNonStorage() {
    _ = consume p1.u3 // expected-error{{'consume' can only be used to partially consume storage}}
                      // expected-note@-1{{non-storage produced by this computed property}}
    _ = consume p1.getUr_NC() // expected-error{{'consume' can only be used to partially consume storage}}
                           // expected-note@-1{{non-storage produced by this call}}
    _ = consume p1[] // expected-error{{'consume' can only be used to partially consume storage}}
                     // expected-note@-1{{non-storage produced by this subscript}}
    _ = consume forward(self) // expected-error{{'consume' can only be used to partially consume storage}}
                              // expected-note@-1{{non-storage produced by this call}}
    _ = consume forward_self() // expected-error{{'consume' can only be used to partially consume storage}}
                               // expected-note@-1{{non-storage produced by this call}}
    _ = consume forward(p1) // expected-error{{'consume' can only be used to partially consume storage}}
                            // expected-note@-1{{non-storage produced by this call}}
    _ = consume p1.forward_self() // expected-error{{'consume' can only be used to partially consume storage}}
                                  // expected-note@-1{{non-storage produced by this call}}
    _ = consume forward(p1.u1) // expected-error{{'consume' can only be used to partially consume storage}}
                               // expected-note@-1{{non-storage produced by this call}}
    _ = consume p1.u1.forward_self() // expected-error{{'consume' can only be used to partially consume storage}}
                                     // expected-note@-1{{non-storage produced by this call}}
  }
}

struct Ur_NC : ~Copyable {
  consuming func forward_self() -> Self { self }
  deinit {}
}
func hold(_ u: borrowing Ur_NC) {}
func take(_ u: consuming Ur_NC) {}
func forward(_ u: consuming Ur_NC) -> Ur_NC { u }

struct Pair_NC : ~Copyable {
  // Storage
  var u1: Ur_NC
  var u2: Ur_NC
  var c: C

  // Not storage
  var u3: Ur_NC {
    Ur_NC()
  }
  func getUr_NC() -> Ur_NC { Ur_NC() }
  subscript() -> Ur_NC { Ur_NC() }
  consuming func forward_self() -> Self { self }
}
func forward(_ u: consuming Pair_NC) -> Pair_NC { u }

struct Quad_NC : ~Copyable {
  var p1: Pair_NC
  var p2: Pair_NC
  consuming func forward_self() -> Self { self }
  deinit {
    _ = consume p1
    _ = consume p2
    _ = consume p1.u1
    _ = consume p1.u2
    _ = consume p2.u1
    _ = consume p2.u2
  }
}
func forward(_ u: consuming Quad_NC) -> Quad_NC { u }

class Container_NC {
  var q: Quad_NC
  init() { fatalError() }
  deinit {
    _ = consume q
    _ = consume q.p1
    _ = consume q.p2
    _ = consume q.p1.u1
    _ = consume q.p1.u2
    _ = consume q.p2.u1
    _ = consume q.p2.u2
  }
}

func decompose(_ c: consuming Container_NC) {
  _ = consume c.q.p1 // expected-error{{'consume' can only be used to partially consume storage}}
                     // expected-note@-1{{non-storage produced by this computed property}}
}

// =============================================================================
// ======================= NONCOPYABLE NON-GENERIC (END) =====================}}
// =============================================================================

// =============================================================================
// ====================== NONCOPYABLE GENERIC (BEGIN) ========================{{
// =============================================================================

extension Quad_NCG where T : ~Copyable {
  consuming func explicitEveryLeaf() {
    _ = consume p1.u1
    _ = consume p1.u1.t
    _ = consume p1.u2
    _ = consume p1.u2.t
    _ = consume p1.c // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p2.u1
    _ = consume p2.u1.t
    _ = consume p2.u2
    _ = consume p2.u2.t
    _ = consume p2.c // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
  }

  consuming func explicitSomeNonStorage() {
    _ = consume p1.u3 // expected-error{{'consume' can only be used to partially consume storage}}
                      // expected-note@-1{{non-storage produced by this computed property}}
    _ = consume p1.getUr_NCG() // expected-error{{'consume' can only be used to partially consume storage}}
                           // expected-note@-1{{non-storage produced by this call}}
    _ = consume p1[] // expected-error{{'consume' can only be used to partially consume storage}}
                     // expected-note@-1{{non-storage produced by this subscript}}
    _ = consume p1[].t // expected-error{{'consume' can only be used to partially consume storage}}
                       // expected-note@-1{{non-storage produced by this subscript}}
    _ = consume forward(self) // expected-error{{'consume' can only be used to partially consume storage}}
                              // expected-note@-1{{non-storage produced by this call}}
    _ = consume forward_self() // expected-error{{'consume' can only be used to partially consume storage}}
                               // expected-note@-1{{non-storage produced by this call}}
    _ = consume forward(p1) // expected-error{{'consume' can only be used to partially consume storage}}
                            // expected-note@-1{{non-storage produced by this call}}
    _ = consume p1.forward_self() // expected-error{{'consume' can only be used to partially consume storage}}
                                  // expected-note@-1{{non-storage produced by this call}}
    _ = consume forward(p1.u1) // expected-error{{'consume' can only be used to partially consume storage}}
                               // expected-note@-1{{non-storage produced by this call}}
    _ = consume p1.u1.forward_self() // expected-error{{'consume' can only be used to partially consume storage}}
                                     // expected-note@-1{{non-storage produced by this call}}
    _ = consume p1.u1.forward_self().t // expected-error{{'consume' can only be used to partially consume storage}}
                                       // expected-note@-1{{non-storage produced by this call}}
  }
}

struct Ur_NCG<T : ~Copyable> : ~Copyable {
  var t: T
  consuming func forward_self() -> Self { self }
  deinit {}
}
func hold<T : ~Copyable>(_ u: borrowing Ur_NCG<T>) {}
func take<T : ~Copyable>(_ u: consuming Ur_NCG<T>) {}
func forward<T : ~Copyable>(_ u: consuming Ur_NCG<T>) -> Ur_NCG<T> { u }

struct Pair_NCG<T : ~Copyable> : ~Copyable {
  // Storage
  var u1: Ur_NCG<T>
  var u2: Ur_NCG<T>
  var c: C

  // Not storage
  var u3: Ur_NCG<T> {
    Ur_NCG(t: get(T.self))
  }
  func getUr_NCG() -> Ur_NCG<T> { Ur_NCG(t: get(T.self)) }
  subscript() -> Ur_NCG<T> { Ur_NCG(t: get(T.self)) }
  consuming func forward_self() -> Self { self }
}
func forward<T : ~Copyable>(_ u: consuming Pair_NCG<T>) -> Pair_NCG<T> { u }

struct Quad_NCG<T : ~Copyable> : ~Copyable {
  var p1: Pair_NCG<T>
  var p2: Pair_NCG<T>
  consuming func forward_self() -> Self { self }
  deinit {
    _ = consume p1
    _ = consume p2
    _ = consume p1.u1
    _ = consume p1.u1.t
    _ = consume p1.u2
    _ = consume p1.u2.t
    _ = consume p2.u1
    _ = consume p2.u1.t
    _ = consume p2.u2
    _ = consume p2.u2.t
  }
}
func forward<T : ~Copyable>(_ u: consuming Quad_NCG<T>) -> Quad_NCG<T> { u }

class Container_NCG<T : ~Copyable> {
  var q: Quad_NCG<T>
  init() { fatalError() }
  deinit {
    _ = consume q.p1
    _ = consume q.p2
    _ = consume q.p1.u1
    _ = consume q.p1.u1.t
    _ = consume q.p1.u2
    _ = consume q.p1.u2.t
    _ = consume q.p2.u1
    _ = consume q.p2.u1.t
    _ = consume q.p2.u2
    _ = consume q.p2.u2.t
  }
}

func decompose<T : ~Copyable>(_ c: consuming Container_NCG<T>) {
  _ = consume c.q.p1 // expected-error{{'consume' can only be used to partially consume storage}}
                     // expected-note@-1{{non-storage produced by this computed property}}
}

// =============================================================================
// ====================== NONCOPYABLE GENERIC (BEGIN) ========================}}
// =============================================================================

// =============================================================================
// ======================== COPYABLE NON-GENERIC (BEGIN) ====================={{
// =============================================================================

extension Quad_C {
  consuming func explicitEveryLeaf() {
    _ = consume p1.u1 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1.u2 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1.c // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p2.u1 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p2.u2 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p2.c // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
  }

  consuming func explicitSomeNonStorage() {
    _ = consume p1.u3 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1.getUr_C() // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1[] // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume forward(self) // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume forward_self() // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume forward(p1) // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1.forward_self() // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume forward(p1.u1) // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1.u1.forward_self() // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
  }
}

struct Ur_C {
  consuming func forward_self() -> Self { self }
}
func hold(_ u: borrowing Ur_C) {}
func take(_ u: consuming Ur_C) {}
func forward(_ u: consuming Ur_C) -> Ur_C { u }

struct Pair_C {
  // Storage
  var u1: Ur_C
  var u2: Ur_C
  var c: C

  // Not storage
  var u3: Ur_C {
    Ur_C()
  }
  func getUr_C() -> Ur_C { Ur_C() }
  subscript() -> Ur_C { Ur_C() }
  consuming func forward_self() -> Self { self }
}
func forward(_ u: consuming Pair_C) -> Pair_C { u }

struct Quad_C {
  var p1: Pair_C
  var p2: Pair_C
  consuming func forward_self() -> Self { self }
}
func forward(_ u: consuming Quad_C) -> Quad_C { u }

class Container_C {
  var q: Quad_C
  init() { fatalError() }
  deinit {
    _ = consume q // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p1 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p2 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p1.u1 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p1.u2 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p2.u1 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p2.u2 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
  }
}

func decompose(_ c: consuming Container_C) {
  _ = consume c.q.p1 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
}

// =============================================================================
// ========================= COPYABLE NON-GENERIC (END) ======================}}
// =============================================================================

// =============================================================================
// ======================== COPYABLE GENERIC (BEGIN) ========================={{
// =============================================================================

extension Quad_CG {
  consuming func explicitEveryLeaf() {
    _ = consume p1.u1 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1.u1.t // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1.u2 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1.u2.t // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1.c // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p2.u1 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p2.u1.t // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p2.u2 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p2.u2.t // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p2.c // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
  }

  consuming func explicitSomeNonStorage() {
    _ = consume p1.u3 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1.getUr_CG() // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1[] // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1[].t // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume forward(self) // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume forward_self() // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume forward(p1) // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1.forward_self() // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume forward(p1.u1) // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1.u1.forward_self() // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume p1.u1.forward_self().t // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
  }
}

struct Ur_CG<T> {
  var t: T
  consuming func forward_self() -> Self { self }
}
func hold<T>(_ u: borrowing Ur_CG<T>) {}
func take<T>(_ u: consuming Ur_CG<T>) {}
func forward<T>(_ u: consuming Ur_CG<T>) -> Ur_CG<T> { u }

struct Pair_CG<T> {
  // Storage
  var u1: Ur_CG<T>
  var u2: Ur_CG<T>
  var c: C

  // Not storage
  var u3: Ur_CG<T> {
    Ur_CG(t: get(T.self))
  }
  func getUr_CG() -> Ur_CG<T> { Ur_CG(t: get(T.self)) }
  subscript() -> Ur_CG<T> { Ur_CG(t: get(T.self)) }
  consuming func forward_self() -> Self { self }
}
func forward<T>(_ u: consuming Pair_CG<T>) -> Pair_CG<T> { u }

struct Quad_CG<T> {
  var p1: Pair_CG<T>
  var p2: Pair_CG<T>
  consuming func forward_self() -> Self { self }
}
func forward<T>(_ u: consuming Quad_CG<T>) -> Quad_CG<T> { u }

class Container_CG<T> {
  var q: Quad_CG<T>
  init() { fatalError() }
  deinit {
    _ = consume q.p1 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p2 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p1.u1 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p1.u1.t // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p1.u2 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p1.u2.t // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p2.u1 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p2.u1.t // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p2.u2 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
    _ = consume q.p2.u2.t // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
  }
}

func decompose<T>(_ c: consuming Container_CG<T>) {
  _ = consume c.q.p1 // expected-error{{'consume' can only be used to partially consume storage of a noncopyable type}}
}

// =============================================================================
// ======================== COPYABLE GENERIC (BEGIN) =========================}}
// =============================================================================
