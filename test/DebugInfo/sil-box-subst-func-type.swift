// RUN: %target-swift-frontend -emit-ir -g %s | %FileCheck %s

func f<T>(_ g: @escaping (() -> T) -> T) -> (() -> T) {
    var h: (() -> T)? = nil
    h = { () -> T in g(h!) }
    return h!
}

// CHECK-LABEL: !DICompositeType(tag: DW_TAG_structure_type, name: "$sxIegr_Sgz_x_lXXD",