
struct Simple<T: Equatable, U: Sequence> {
  func simple<V>(v: V) where V: Hashable {}
  func assoc<V>(v: V, w: T, p: U) where U.Element == T, V: Equatable {}
}

extension Simple where T == Int {
  func concrete(t: T) {}
  func testCall(x: U) where U.Element == T {
    assoc(v: "hello", w: 1, p: x)
  }
}

func foo() {
  Simple<String, [String]>.init().assoc(v: 1, w: "a", p: ["hello"])
}


// RUN:  %sourcekitd-test -req=cursor -pos=3:8 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=SIMPLE %s
// RUN:  %sourcekitd-test -req=cursor -pos=4:8 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=ASSOC %s
// RUN:  %sourcekitd-test -req=cursor -pos=8:8 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=CONCRETE %s
// RUN:  %sourcekitd-test -req=cursor -pos=10:5 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=ASSOCCALL_EXT  %s
// RUN:  %sourcekitd-test -req=cursor -pos=15:35 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=ASSOCCALL_MEMBER  %s

// SIMPLE: "functionSignature": {
// SIMPLE:         "spelling": "v"
// SIMPLE:         "spelling": ": "
// SIMPLE:         "spelling": "V"
// SIMPLE: }
// SIMPLE: "swiftGenerics": {
// SIMPLE:   "constraints": [
// SIMPLE:     {
// SIMPLE:       "kind": "conformance",
// SIMPLE:       "lhs": "T",
// SIMPLE:       "rhs": "Equatable"
// SIMPLE:     },
// SIMPLE:     {
// SIMPLE:       "kind": "conformance",
// SIMPLE:       "lhs": "U",
// SIMPLE:       "rhs": "Sequence"
// SIMPLE:     },
// SIMPLE:     {
// SIMPLE:       "kind": "conformance",
// SIMPLE:       "lhs": "V",
// SIMPLE:       "rhs": "Hashable"
// SIMPLE:     }
// SIMPLE:   ],
// SIMPLE:   "parameters": [
// SIMPLE:     {
// SIMPLE:       "depth": 0,
// SIMPLE:       "index": 0,
// SIMPLE:       "name": "T"
// SIMPLE:     },
// SIMPLE:     {
// SIMPLE:       "depth": 0,
// SIMPLE:       "index": 1,
// SIMPLE:       "name": "U"
// SIMPLE:     },
// SIMPLE:     {
// SIMPLE:       "depth": 1,
// SIMPLE:       "index": 0,
// SIMPLE:       "name": "V"
// SIMPLE:     }
// SIMPLE:   ]
// SIMPLE: }


// ASSOC: "functionSignature": {
// ASSOC:         "spelling": "v"
// ASSOC:         "spelling": ": "
// ASSOC:         "spelling": "V"
// ASSOC:         "spelling": "w"
// ASSOC:         "spelling": ": "
// ASSOC:         "spelling": "T"
// ASSOC:         "spelling": "p"
// ASSOC:         "spelling": ": "
// ASSOC:         "spelling": "U"
// ASSOC: }
// ASSOC: "swiftGenerics": {
// ASSOC:   "constraints": [
// ASSOC:     {
// ASSOC:       "kind": "conformance",
// ASSOC:       "lhs": "T",
// ASSOC:       "rhs": "Equatable"
// ASSOC:     },
// ASSOC:     {
// ASSOC:       "kind": "sameType",
// ASSOC:       "lhs": "T",
// ASSOC:       "rhs": "U.Element"
// ASSOC:     },
// ASSOC:     {
// ASSOC:       "kind": "conformance",
// ASSOC:       "lhs": "U",
// ASSOC:       "rhs": "Sequence"
// ASSOC:     }
// ASSOC:   ],
// ASSOC:   "parameters": [
// ASSOC:     {
// ASSOC:       "depth": 0,
// ASSOC:       "index": 0,
// ASSOC:       "name": "T"
// ASSOC:     },
// ASSOC:     {
// ASSOC:       "depth": 0,
// ASSOC:       "index": 1,
// ASSOC:       "name": "U"
// ASSOC:     },
// ASSOC:     {
// ASSOC:       "depth": 1,
// ASSOC:       "index": 0,
// ASSOC:       "name": "V"
// ASSOC:     }
// ASSOC:   ]
// ASSOC: }


// CONCRETE: "functionSignature": {
// CONCRETE:         "spelling": "t"
// CONCRETE:         "spelling": ": "
// CONCRETE:         "spelling": "T"
// CONCRETE: }
// CONCRETE: "swiftGenerics": {
// CONCRETE:   "constraints": [
// CONCRETE:     {
// CONCRETE:       "kind": "sameType",
// CONCRETE:       "lhs": "T",
// CONCRETE:       "rhs": "Int"
// CONCRETE:     },
// CONCRETE:     {
// CONCRETE:       "kind": "conformance",
// CONCRETE:       "lhs": "U",
// CONCRETE:       "rhs": "Sequence"
// CONCRETE:     }
// CONCRETE:   ],
// CONCRETE:   "parameters": [
// CONCRETE:     {
// CONCRETE:       "depth": 0,
// CONCRETE:       "index": 0,
// CONCRETE:       "name": "T"
// CONCRETE:     },
// CONCRETE:     {
// CONCRETE:       "depth": 0,
// CONCRETE:       "index": 1,
// CONCRETE:       "name": "U"
// CONCRETE:     }
// CONCRETE:   ]
// CONCRETE: }


// ASSOCCALL_EXT: "functionSignature": {
// ASSOCCALL_EXT:         "spelling": "v"
// ASSOCCALL_EXT:         "spelling": ": "
// ASSOCCALL_EXT:         "spelling": "V"
// ASSOCCALL_EXT:         "spelling": "w"
// ASSOCCALL_EXT:         "spelling": ": "
// ASSOCCALL_EXT:         "spelling": "Int"
// ASSOCCALL_EXT:         "spelling": "p"
// ASSOCCALL_EXT:         "spelling": ": "
// ASSOCCALL_EXT:         "spelling": "U"
// ASSOCCALL_EXT: "swiftGenerics": {
// ASSOCCALL_EXT:   "constraints": [
// ASSOCCALL_EXT:     {
// ASSOCCALL_EXT:       "kind": "conformance",
// ASSOCCALL_EXT:       "lhs": "U",
// ASSOCCALL_EXT:       "rhs": "Sequence"
// ASSOCCALL_EXT:     },
// ASSOCCALL_EXT:     {
// ASSOCCALL_EXT:       "kind": "conformance",
// ASSOCCALL_EXT:       "lhs": "V",
// ASSOCCALL_EXT:       "rhs": "Equatable"
// ASSOCCALL_EXT:     }
// ASSOCCALL_EXT:   ],
// ASSOCCALL_EXT:   "parameters": [
// ASSOCCALL_EXT:     {
// ASSOCCALL_EXT:       "depth": 0,
// ASSOCCALL_EXT:       "index": 1,
// ASSOCCALL_EXT:       "name": "U"
// ASSOCCALL_EXT:     },
// ASSOCCALL_EXT:     {
// ASSOCCALL_EXT:       "depth": 1,
// ASSOCCALL_EXT:       "index": 0,
// ASSOCCALL_EXT:       "name": "V"
// ASSOCCALL_EXT:     }
// ASSOCCALL_EXT:   ]
// ASSOCCALL_EXT: }


// ASSOCCALL_MEMBER: "functionSignature": {
// ASSOCCALL_MEMBER:         "spelling": "v"
// ASSOCCALL_MEMBER:         "spelling": ": "
// ASSOCCALL_MEMBER:         "spelling": "V"
// ASSOCCALL_MEMBER:         "spelling": "w"
// ASSOCCALL_MEMBER:         "spelling": ": "
// ASSOCCALL_MEMBER:         "spelling": "String"
// ASSOCCALL_MEMBER:         "spelling": "p"
// ASSOCCALL_MEMBER:         "spelling": ": ["
// ASSOCCALL_MEMBER:         "spelling": "String"
// ASSOCCALL_MEMBER:         "spelling": "]"
// ASSOCCALL_MEMBER: },
// ASSOCCALL_MEMBER: "swiftGenerics": {
// ASSOCCALL_MEMBER:   "constraints": [
// ASSOCCALL_MEMBER:     {
// ASSOCCALL_MEMBER:       "kind": "conformance",
// ASSOCCALL_MEMBER:       "lhs": "V",
// ASSOCCALL_MEMBER:       "rhs": "Equatable"
// ASSOCCALL_MEMBER:     }
// ASSOCCALL_MEMBER:   ],
// ASSOCCALL_MEMBER:   "parameters": [
// ASSOCCALL_MEMBER:     {
// ASSOCCALL_MEMBER:       "depth": 1,
// ASSOCCALL_MEMBER:       "index": 0,
// ASSOCCALL_MEMBER:       "name": "V"
// ASSOCCALL_MEMBER:     }
// ASSOCCALL_MEMBER:   ]
// ASSOCCALL_MEMBER: }
