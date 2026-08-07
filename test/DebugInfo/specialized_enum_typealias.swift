// RUN: %target-swift-frontend %s -emit-ir -gdwarf-types -o - | %FileCheck %s

enum MyOptional<Element> {
    case some(Element)
    case none
}

struct Container<Element> {
    typealias Handler = () -> Void
    var handler: MyOptional<Handler> = .none
}

func test() {
    let c = Container<Int>()
}

test()

// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, {{.*}}templateParams: ![[PARAMS:[0-9]+]], identifier: "$s{{.*}}MyOptional{{.*}}
// CHECK-DAG: ![[PARAMS]] = !{![[PARAM:[0-9]+]]}
// CHECK-DAG: ![[PARAM]] = !DITemplateTypeParameter(type: ![[HANDLER:[0-9]+]])
// CHECK-DAG: ![[HANDLER]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s{{.*}}Container{{.*}}Handler{{.*}}"
