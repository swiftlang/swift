protocol Entity {}
struct Empty: Entity {
    var value: Void = ()
}

@_functionBuilder
struct Builder {
  static func buildBlock() ->  { Empty() }
  static func buildBlock<T: Entity>(_ t: T) -> T { t }
}

struct MyValue {
    var id: Int
    var title: String
}

func build(_ arg: (MyValue) -> String) -> Empty { Empty() }

struct MyStruct {
  @Builder var body: some Entity {
    build { value in
      value./*HERE * 2*/
    } /*HERE*/
  }
}

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=22:13 %s -- %s == \
// RUN:   -req=complete -pos=22:13 %s -- %s == \
// RUN:   -req=complete -pos=23:7 %s -- %s \
// RUN: | tee %t.result | %FileCheck %s

// CHECK-LABEL: key.results: [
// CHECK-DAG: key.description: "id"
// CHECK-DAG: key.description: "title"
// CHECK-DAG: key.description: "self"
// CHECK: ]
// CHECK-NOT: key.reusingastcontext: 1 

// CHECK-LABEL: key.results: [
// CHECK-DAG: key.description: "id"
// CHECK-DAG: key.description: "title"
// CHECK-DAG: key.description: "self"
// CHECK: ]
// CHECK: key.reusingastcontext: 1 

// CHECK-LABEL: key.results: [
// CHECK-DAG: key.description: "value"
// CHECK: ]
// CHECK: key.reusingastcontext: 1 

