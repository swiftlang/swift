// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck -check-prefix=CHECK %s
// RUN: %target-swift-ide-test -print-indexed-symbols -include-locals  -source-filename %s | %FileCheck -check-prefix=LOCAL %s

func foo(a: Int, b: Int, c: Int) {
  // CHECK-NOT: [[@LINE-1]]:10 | function/acc-get{{.*}}/Swift | getter:a
  // CHECK-NOT: [[@LINE-1]]:10 | function/acc-set{{.*}}/Swift | setter:a

    let x = a + b
    // LOCAL: [[@LINE-1]]:9 | variable(local)/Swift | x | [[x_USR:.*]] | Def,RelChild | rel: 1
    // CHECK-NOT: [[@LINE-2]]:9 | variable(local)/Swift | x | {{.*}} | Def,RelChild | rel: 1
    // LOCAL-NOT: [[@LINE-3]]:13 | function/acc-get{{.*}}/Swift | getter:a

    let y = x + c
    // LOCAL: [[@LINE-1]]:9 | variable(local)/Swift | y | [[y_USR:.*]] | Def,RelChild | rel: 1
    // CHECK-NOT: [[@LINE-2]]:9 | variable(local)/Swift | y | {{.*}} | Def,RelChild | rel: 1
    // LOCAL: [[@LINE-3]]:13 | variable(local)/Swift | x | [[x_USR]] | Ref,Read,RelCont | rel: 1
    // CHECK-NOT: [[@LINE-4]]:13 | variable(local)/Swift | x | {{.*}} | Ref,Read,RelCont | rel: 1

    struct LocalStruct {
        // LOCAL: [[@LINE-1]]:12 | struct(local)/Swift | LocalStruct | [[LocalStruct_USR:.*]] | Def,RelChild | rel: 1
        // CHECK-NOT: [[@LINE-2]]:12 | struct(local)/Swift | LocalStruct | {{.*}} | Def,RelChild | rel: 1

        let member = 2
        // LOCAL: [[@LINE-1]]:13 | instance-property(local)/Swift | member | {{.*}} | Def,RelChild | rel: 1
        // LOCAL-NEXT: RelChild | struct(local)/Swift | LocalStruct | [[LocalStruct_USR]]
        // CHECK-NOT: [[@LINE-3]]:13 | instance-property(local)/Swift | member | {{.*}} | Def,RelChild | rel: 1
    }

    enum LocalEnum {
        // LOCAL: [[@LINE-1]]:10 | enum(local)/Swift | LocalEnum | [[LocalEnum_USR:.*]] | Def,RelChild | rel: 1
        // CHECK-NOT: [[@LINE-2]]:10 | enum(local)/Swift | LocalEnum | {{.*}} | Def,RelChild | rel: 1

        case foo(x: LocalStruct)
        // LOCAL: [[@LINE-1]]:14 | enumerator(local)/Swift | foo(x:) | [[LocalEnum_foo_USR:.*]] | Def,RelChild | rel: 1
        // CHECK-NOT: [[@LINE-2]]:14 | enumerator(local)/Swift | foo(x:) | {{.*}} | Def,RelChild | rel: 1
        // LOCAL: [[@LINE-3]]:21 | struct(local)/Swift | LocalStruct | [[LocalStruct_USR]] | Ref,RelCont | rel: 1
    }

    let _ = LocalEnum.foo(x: LocalStruct())
    // LOCAL: [[@LINE-1]]:13 | enum(local)/Swift | LocalEnum | [[LocalEnum_USR]] | Ref,RelCont | rel: 1
    // CHECK-NOT: [[@LINE-2]]:13 | enum(local)/Swift | LocalEnum | {{.*}} | Ref,RelCont | rel: 1
    // LOCAL: [[@LINE-3]]:23 | enumerator(local)/Swift | foo(x:) | [[LocalEnum_foo_USR]] | Ref,Call,RelCall,RelCont | rel: 1
    // CHECK-NOT: [[@LINE-4]]:23 | enumerator(local)/Swift | foo(x:) | {{.*}} | Ref,RelCont | rel: 1
    // LOCAL: [[@LINE-5]]:30 | struct(local)/Swift | LocalStruct | [[LocalStruct_USR]] | Ref,RelCont | rel: 1
    // CHECK-NOT: [[@LINE-6]]:30 | struct(local)/Swift | LocalStruct | {{.*}} | Ref,RelCont | rel: 1

}

func bar(arg: Int?) {
  switch arg {
  case let .some(x) where x == 0:
    // LOCAL: [[@LINE-1]]:18 | variable(local)/Swift | x | [[x_USR:.*]] | Def,RelChild | rel: 1
    // LOCAL: [[@LINE-2]]:27 | variable(local)/Swift | x | [[x_USR]] | Ref,Read,RelCont | rel: 1
    // CHECK-NOT: [[@LINE-3]]:18 | variable(local)/Swift | x | {{.*}} | Def,RelChild | rel: 1
    // CHECK-NOT: [[@LINE-4]]:27 | variable(local)/Swift | x | {{.*}} | Ref,Read,RelCont | rel: 1
    print(x)
    // LOCAL: [[@LINE-1]]:11 | variable(local)/Swift | x | [[x_USR]] | Ref,Read,RelCont | rel: 1
    // CHECK-NOT: [[@LINE-2]]:11 | variable(local)/Swift | x | {{.*}} | Ref,Read,RelCont | rel: 1

  case let .some(x) where x == 1,
    // LOCAL: [[@LINE-1]]:18 | variable(local)/Swift | x | [[x2_USR:.*]] | Def,RelChild | rel: 1
    // LOCAL: [[@LINE-2]]:27 | variable(local)/Swift | x | [[x2_USR]] | Ref,Read,RelCont | rel: 1
    // CHECK-NOT: [[@LINE-3]]:18 | variable(local)/Swift | x | {{.*}} | Def,RelChild | rel: 1
    // CHECK-NOT: [[@LINE-4]]:27 | variable(local)/Swift | x | {{.*}} | Ref,Read,RelCont | rel: 1
       let .some(x) where x == 2:
    // LOCAL: [[@LINE-1]]:18 | variable(local)/Swift | x | [[x2_USR]] | Def,RelChild | rel: 1
    // LOCAL: [[@LINE-2]]:27 | variable(local)/Swift | x | [[x2_USR]] | Ref,Read,RelCont | rel: 1
    // CHECK-NOT: [[@LINE-3]]:18 | variable(local)/Swift | x | {{.*}} | Def,RelChild | rel: 1
    // CHECK-NOT: [[@LINE-4]]:27 | variable(local)/Swift | x | {{.*}} | Ref,Read,RelCont | rel: 1
    print(x)
    // LOCAL: [[@LINE-1]]:11 | variable(local)/Swift | x | [[x2_USR]] | Ref,Read,RelCont | rel: 1
    // CHECK-NOT: [[@LINE-2]]:11 | variable(local)/Swift | x | {{.*}} | Ref,Read,RelCont | rel: 1
    fallthrough
  case let .some(x) where x == 3:
    // LOCAL: [[@LINE-1]]:18 | variable(local)/Swift | x | [[x2_USR]] | Def,RelChild | rel: 1
    // LOCAL: [[@LINE-2]]:27 | variable(local)/Swift | x | [[x2_USR]] | Ref,Read,RelCont | rel: 1
    // CHECK-NOT: [[@LINE-1]]:18 | variable(local)/Swift | x | {{.*}} | Def,RelChild | rel: 1
    // CHECK-NOT: [[@LINE-2]]:27 | variable(local)/Swift | x | {{.*}} | Ref,Read,RelCont | rel: 1
    print(x)
    // LOCAL: [[@LINE-1]]:11 | variable(local)/Swift | x | [[x2_USR]] | Ref,Read,RelCont | rel: 1
    // CHECK-NOT: [[@LINE-1]]:11 | variable(local)/Swift | x | {{.*}} | Ref,Read,RelCont | rel: 1
  default:
    break
  }
}
