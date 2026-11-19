// RUN: %target-swift-frontend %s -emit-ir -g -module-name a -o %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck %s --check-prefix=NEGATIVE < %t.ll

// SIL type lowering changes the types of some variables, for example by giving
// metatypes a representation: `MyInt.Type` becomes `@thin Int.Type`. The
// debug info should still use the typealiases the variables were declared
// with, for the parts of the type that lowering didn't change.

struct Box<T> { var t: T }
enum Tag<Owner> {}
typealias MyInt = Int
typealias Flag = Bool
typealias Meta = Int.Type
typealias MetaT = MyInt.Type
typealias BoxA<T> = Box<T>
typealias Const<T> = Int
protocol P {}
typealias PA = any P
struct R {
  typealias Boxed = Box<Int>
  typealias T = Tag<R>
}

func use<T>(_ t: T) {}

public func f() {
  // CHECK-DAG: !DILocalVariable(name: "value",{{.*}} type: ![[VALUE:[0-9]+]]
  // CHECK-DAG: ![[VALUE]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[VALUE_TY:[0-9]+]])
  // CHECK-DAG: ![[VALUE_TY]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s1a5MyIntaD"
  let value: MyInt = 1

  // CHECK-DAG: !DILocalVariable(name: "c",{{.*}} type: ![[C:[0-9]+]]
  // CHECK-DAG: ![[C]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[C_TY:[0-9]+]])
  // CHECK-DAG: ![[C_TY]] = !DICompositeType({{.*}}name: "$s1a5MyIntaXMtD"
  let c: MyInt.Type = MyInt.self

  // CHECK-DAG: !DILocalVariable(name: "d",{{.*}} type: ![[D:[0-9]+]]
  // CHECK-DAG: ![[D]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[D_TY:[0-9]+]])
  // CHECK-DAG: ![[D_TY]] = !DICompositeType({{.*}}name: "$s1a1RV5BoxedaXMtD"
  let d: R.Boxed.Type = R.Boxed.self

  // R.T is only referenced from within a metatype, which is described by its
  // mangled name alone. The debugger needs a typedef to resolve it.
  // CHECK-DAG: !DILocalVariable(name: "e",{{.*}} type: ![[E:[0-9]+]]
  // CHECK-DAG: ![[E]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[E_TY:[0-9]+]])
  // CHECK-DAG: ![[E_TY]] = !DICompositeType({{.*}}name: "$s1a1RV1TaXMtD"
  // CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$s1a1RV1TaD"
  let e: R.T.Type = R.T.self

  // CHECK-DAG: !DILocalVariable(name: "p",{{.*}} type: ![[P:[0-9]+]]
  // CHECK-DAG: ![[P]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[P_TY:[0-9]+]])
  // CHECK-DAG: ![[P_TY]] = !DICompositeType({{.*}}name: "$s1a2PAaXMtD"
  let p: PA.Type = PA.self

  // CHECK-DAG: !DICompositeType({{.*}}name: "$s1a5MyIntaXMtSgD"
  let o: MyInt.Type? = MyInt.self

  // Flag is only referenced from within a tuple, which is described by its
  // mangled name alone. The debugger needs a typedef to resolve it.
  // CHECK-DAG: !DILocalVariable(name: "tup",{{.*}} type: ![[TUP:[0-9]+]]
  // CHECK-DAG: ![[TUP]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[TUP_TY:[0-9]+]])
  // CHECK-DAG: ![[TUP_TY]] = !DICompositeType({{.*}}name: "$s1a4Flaga_ACXMttD"
  // CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$s1a4FlagaD"
  let tup: (Flag, Flag.Type) = (true, Flag.self)

  // Int is spelled both as MyInt and as Int, so neither spelling is used.
  // CHECK-DAG: !DILocalVariable(name: "mixed",{{.*}} type: ![[MIXED:[0-9]+]]
  // CHECK-DAG: ![[MIXED]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[MIXED_TY:[0-9]+]])
  // CHECK-DAG: ![[MIXED_TY]] = !DICompositeType({{.*}}name: "$sSiXMt_SitD"
  let mixed: (MyInt.Type, Int) = (MyInt.self, 2)

  // Meta stands for `Int.Type`, which doesn't match the lowered
  // `@thin Int.Type`.
  // CHECK-DAG: !DILocalVariable(name: "m",{{.*}} type: ![[M:[0-9]+]]
  // CHECK-DAG: ![[M]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[M_TY:[0-9]+]])
  // CHECK-DAG: ![[M_TY]] = !DICompositeType({{.*}}name: "$sSiXMtD"
  let m: Meta = Int.self

  // MetaT can't be used for the same reason, but the MyInt inside of it can.
  // CHECK-DAG: !DILocalVariable(name: "mt",{{.*}} type: ![[MT:[0-9]+]]
  // CHECK-DAG: ![[MT]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[MT_TY:[0-9]+]])
  // CHECK-DAG: ![[MT_TY]] = !DICompositeType({{.*}}name: "$s1a5MyIntaXMt_SbtD"
  let mt: (MetaT, Bool) = (MyInt.self, true)

  use(value); use(c); use(d); use(e); use(p); use(o); use(tup); use(mixed)
  use(m); use(mt)
}

func g<T>(_ x: T) {
  // Sugar that depends on a generic parameter is not used, because LLDB can't
  // resolve it.
  // NEGATIVE-NOT: 4BoxAa
  let gA: BoxA<T> = Box(t: x)

  // CHECK-DAG: !DILocalVariable(name: "gC",{{.*}} type: ![[GC:[0-9]+]]
  // CHECK-DAG: ![[GC]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[GC_TY:[0-9]+]])
  // CHECK-DAG: ![[GC_TY]] = !DICompositeType({{.*}}name: "$s1a5MyIntaXMt_xtD"
  let gC: (MyInt.Type, T) = (MyInt.self, x)

  // Const<T> mentions T even though its canonical type doesn't, so it can't be
  // used either.
  // CHECK-DAG: !DILocalVariable(name: "phantom",{{.*}} type: ![[PHANTOM:[0-9]+]]
  // CHECK-DAG: ![[PHANTOM]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[PHANTOM_TY:[0-9]+]])
  // CHECK-DAG: ![[PHANTOM_TY]] = !DICompositeType({{.*}}name: "$sx_SitD"
  let phantom: (T, Const<T>) = (x, 1)
  use(gA); use(gC); use(phantom)
}

struct Outer<T> {
  struct Inner { typealias Primitive = Int }

  func f(_ x: T) {
    // Inner.Primitive mentions T through its parent.
    // CHECK-DAG: !DILocalVariable(name: "nested",{{.*}} type: ![[NESTED:[0-9]+]]
    // CHECK-DAG: ![[NESTED]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[NESTED_TY:[0-9]+]])
    // CHECK-DAG: ![[NESTED_TY]] = !DICompositeType({{.*}}name: "$sSi_xtD"
    let nested: (Inner.Primitive, T) = (1, x)
    use(nested)
  }
}
