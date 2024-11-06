// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

struct Foo: Hashable {}

_ = Array<Int>(repeating: 0, count: 1)
// CHECK: [[@LINE-1]]:5 | constructor/Swift | init(repeating:count:) | s:Sa9repeating5countSayxGx_Sitcfc | {{.*}}Ref
_ = [Int](repeating: 0, count: 1)
// CHECK: [[@LINE-1]]:5 | constructor/Swift | init(repeating:count:) | s:Sa9repeating5countSayxGx_Sitcfc | {{.*}}Ref
_ = Array<Foo>(repeating: Foo(), count: 1)
// CHECK: [[@LINE-1]]:5 | constructor/Swift | init(repeating:count:) | s:Sa9repeating5countSayxGx_Sitcfc | {{.*}}Ref
// CHECK: [[@LINE-2]]:27 | constructor/Swift | init() | s:14swift_ide_test3FooVACycfc | Ref,Call
_ = [Foo](repeating: Foo(), count: 1)
// CHECK: [[@LINE-1]]:5 | constructor/Swift | init(repeating:count:) | s:Sa9repeating5countSayxGx_Sitcfc | {{.*}}Ref
// CHECK: [[@LINE-2]]:22 | constructor/Swift | init() | s:14swift_ide_test3FooVACycfc | Ref,Call

_ = Dictionary<Foo, String>(minimumCapacity: 1)
// CHECK: [[@LINE-1]]:5 | constructor/Swift | init(minimumCapacity:) | s:SD15minimumCapacitySDyxq_GSi_tcfc | {{.*}}Ref
_ = [Foo: String](minimumCapacity: 1)
// CHECK: [[@LINE-1]]:5 | constructor/Swift | init(minimumCapacity:) | s:SD15minimumCapacitySDyxq_GSi_tcfc | {{.*}}Ref
_ = [String: Int](uniqueKeysWithValues: zip(["one", "two", "three"], 1...3))
// CHECK: [[@LINE-1]]:5 | constructor/Swift | init(uniqueKeysWithValues:) | s:SD20uniqueKeysWithValuesSDyxq_Gqd__n_tcSTRd__x_q_t7ElementRtd__lufc | {{.*}}Ref

extension Array where Element == Int {
// CHECK: [[@LINE+1]]:3 | constructor/Swift | init(_:) | s:Sa14swift_ide_testSiRszlEySaySiGSicfc | {{.*}}Def
  init(_ input: Int) {
    self = [input]
  }
}

_ = [Int](0)
// CHECK: [[@LINE-1]]:5 | constructor/Swift | init(_:) | s:Sa14swift_ide_testSiRszlEySaySiGSicfc | {{.*}}Ref

extension Dictionary {
// CHECK: [[@LINE+1]]:3 | constructor/Swift | init(_:_:) | s:SD14swift_ide_testEySDyxq_Gx_q_tcfc | {{.*}}Def
  init(_ k: Key, _ v: Value) {
    self = [k: v]
  }
}

_ = [Int: Int](0, 1)
// CHECK: [[@LINE-1]]:5 | constructor/Swift | init(_:_:) | s:SD14swift_ide_testEySDyxq_Gx_q_tcfc | {{.*}}Ref
