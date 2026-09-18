// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractChainedMemberReferences.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractChainedMemberReferences.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}

// Two-level chain infrastructure: static root → instance step/other

@_marker protocol OuterProto {}
@_marker protocol InnerProto: OuterProto {}

struct Concrete: OuterProto {}
extension Concrete: InnerProto {}

extension OuterProto where Self == Concrete {
    static var root: some InnerProto {
        Int.random(in: 0...1) > 0 ? Concrete() : Concrete()
    }
}

extension InnerProto {
    var step: some OuterProto {
        Int.random(in: 0...1) > 0 ? Concrete() : Concrete()
    }
    var other: some OuterProto {
        Concrete()
    }
}

struct Wrapper {
    init(_ v: some OuterProto) {}
}

// Three-level chain infrastructure: static base → instance mid → instance leaf

@_marker protocol TopProto {}
@_marker protocol MidProto: TopProto {}

struct Node: TopProto {}
extension Node: MidProto {}

extension TopProto where Self == Node {
    static var base: some MidProto {
        Int.random(in: 0...1) > 0 ? Node() : Node()
    }
}

extension MidProto {
    var mid: some TopProto {
        Int.random(in: 0...1) > 0 ? Node() : Node()
    }
}

extension TopProto {
    var leaf: some TopProto {
        Node()
    }
}

struct NodeWrapper {
    init(_ v: some TopProto) {}
}

// --------------------------------------------------------------------------

struct Container: MyProto {
    // Double-hop: .root (MemberReference) → .step (instance var)
    var prop1 = Wrapper(.root.step)

    // Double-hop: .root (MemberReference) → .other (instance var)
    var prop2 = Wrapper(.root.other)

    // Triple-hop: .base (MemberReference) → .mid (instance var) → .leaf (instance var)
    var prop3 = NodeWrapper(.base.mid.leaf)
}

// CHECK:       "label": "prop1",
// CHECK:       "valueKind": "InitCall",
// CHECK:       "label": "",
// CHECK:       "valueKind": "ChainedMemberReference",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "baseValue": {
// CHECK-NEXT:      "valueKind": "MemberReference",
// CHECK-NEXT:      "value": {
// CHECK-NEXT:        "baseType": "ExtractChainedMemberReferences.Concrete",
// CHECK-NEXT:        "memberLabel": "root"
// CHECK-NEXT:      }
// CHECK-NEXT:    },
// CHECK-NEXT:    "members": [
// CHECK-NEXT:      "step"
// CHECK-NEXT:    ]
// CHECK-NEXT:  }

// CHECK:       "label": "prop2",
// CHECK:       "valueKind": "InitCall",
// CHECK:       "label": "",
// CHECK:       "valueKind": "ChainedMemberReference",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "baseValue": {
// CHECK-NEXT:      "valueKind": "MemberReference",
// CHECK-NEXT:      "value": {
// CHECK-NEXT:        "baseType": "ExtractChainedMemberReferences.Concrete",
// CHECK-NEXT:        "memberLabel": "root"
// CHECK-NEXT:      }
// CHECK-NEXT:    },
// CHECK-NEXT:    "members": [
// CHECK-NEXT:      "other"
// CHECK-NEXT:    ]
// CHECK-NEXT:  }

// CHECK:       "label": "prop3",
// CHECK:       "valueKind": "InitCall",
// CHECK:       "label": "",
// CHECK:       "valueKind": "ChainedMemberReference",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "baseValue": {
// CHECK-NEXT:      "valueKind": "MemberReference",
// CHECK-NEXT:      "value": {
// CHECK-NEXT:        "baseType": "ExtractChainedMemberReferences.Node",
// CHECK-NEXT:        "memberLabel": "base"
// CHECK-NEXT:      }
// CHECK-NEXT:    },
// CHECK-NEXT:    "members": [
// CHECK-NEXT:      "mid",
// CHECK-NEXT:      "leaf"
// CHECK-NEXT:    ]
// CHECK-NEXT:  }
