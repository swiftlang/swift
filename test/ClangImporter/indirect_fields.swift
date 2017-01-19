// RUN: %target-swift-frontend -typecheck -sdk "" -I %t -I %S/Inputs/custom-modules %s -verify

import IndirectFields

func build_struct(a: Int32, c: Int32, d: Int32) -> StructWithIndirectField {
    return StructWithIndirectField(__Anonymous_field0: .init(a: a), c: c, d: d)
}

func build_struct(b: Int32, c: Int32, d: Int32) -> StructWithIndirectField {
    return StructWithIndirectField(__Anonymous_field0: .init(b: b), c: c, d: d)
}

func build_union(a: Int32, b: Int32) -> UnionWithIndirectField {
    return UnionWithIndirectField(__Anonymous_field0: .init(a: a, b: b))
}

func build_union(c: Int32) -> UnionWithIndirectField {
    return UnionWithIndirectField(c: c)
}
