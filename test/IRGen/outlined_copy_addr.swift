// RUN: %target-swift-frontend -emit-ir  -module-name outcopyaddr -primary-file %s | %FileCheck %s

public protocol BaseProt {
}

public protocol ChildProt: BaseProt {
}

public struct BaseStruct<T: BaseProt> {
    public typealias Element = T
    public var elem1: Element
    public var elem2: Element
}

public struct StructWithBaseStruct<T: BaseProt> {
    public typealias Element = T
    var elem1: Element
    var elem2: BaseStruct<Element>
}

// CHECK-LABEL: define hidden swiftcc void @"$S11outcopyaddr010StructWithbc4BaseB0V4elemAA0bcdB0VyxGvg"(%T11outcopyaddr014StructWithBaseB0V.0* noalias nocapture sret, %swift.type* %"StructWithStructWithBaseStruct<T>", %T11outcopyaddr010StructWithbc4BaseB0V* noalias nocapture swiftself)
// CHECK: call %T11outcopyaddr014StructWithBaseB0V.0* @"$S11outcopyaddr014StructWithBaseB0VyxGAA9ChildProtRzlWOc"
public struct StructWithStructWithBaseStruct<T: ChildProt> {
    public typealias Element = T
    let elem: StructWithBaseStruct<Element>
}
