// RUN: %target-swift-frontend -typecheck -sdk "" -I %t -I %S/Inputs/custom-modules %s -verify

import IndirectFields

func build_struct(a: Int32, c: Int32, d: Int32) -> StructWithIndirectField {
    return StructWithIndirectField(.init(a: a), c: c, d: d)
}

func build_struct(b: Int32, c: Int32, d: Int32) -> StructWithIndirectField {
    return StructWithIndirectField(.init(b: b), c: c, d: d)
}

func build_union(a: Int32, b: Int32) -> UnionWithIndirectField {
    return UnionWithIndirectField(.init(a: a, b: b))
}

func build_union(c: Int32) -> UnionWithIndirectField {
    return UnionWithIndirectField(c: c)
}

func build_deep(a: Int32, b: Int32) -> DeepIndirectField {
    return DeepIndirectField(.init(.init(a: a, b: b)))
}

func build_deep(c: Int32, d: Int32) -> DeepIndirectField {
    return DeepIndirectField(.init(.init(c: c, d: d)))
}
