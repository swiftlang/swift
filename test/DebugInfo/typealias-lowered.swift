// RUN: %target-swift-frontend %s -emit-ir -g -module-name a -o %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck %s --check-prefix=NEGATIVE < %t.ll
// RUN: %target-swift-frontend %s -emit-ir -g -O -module-name a -o %t.opt.ll
// RUN: %FileCheck %s --check-prefix=OPT < %t.opt.ll

// SIL type lowering changes the types of some variables, for example by giving
// metatypes a representation: `MyInt.Type` becomes `@thin Int.Type`. The
// debug info should still use the typealiases the variables were declared
// with, for the parts of the type that lowering didn't change. Generic
// specialization and opaque type substitution also change variable types, by
// replacing generic parameters and opaque types with concrete types.

struct Box<T> { var t: T }
class Ref<A, B> {}
enum Tag<Owner> {}
typealias MyInt = Int
// Each of these is used by only one test case, so that no other case can make
// its check pass.
typealias Flag = Bool
typealias Toggle = Bool
typealias Byte = Int8
typealias Short = Int16
typealias Elem = Int32
typealias Kind = UInt8
typealias Meta = Int.Type
typealias MetaT = MyInt.Type
typealias BoxA<T> = Box<T>
typealias Const<T> = Int
protocol P {}
extension Int: P {}
typealias PA = any P
struct R {
  typealias Boxed = Box<Int>
  typealias T = Tag<R>
}

@inline(never) @_optimize(none)
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

  // Int is spelled both as MyInt and as Int. Each keeps its own spelling.
  // CHECK-DAG: !DILocalVariable(name: "mixed",{{.*}} type: ![[MIXED:[0-9]+]]
  // CHECK-DAG: ![[MIXED]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[MIXED_TY:[0-9]+]])
  // CHECK-DAG: ![[MIXED_TY]] = !DICompositeType({{.*}}name: "$s1a5MyIntaXMt_SitD"
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

  // The opaque type is replaced by its underlying type, Int, which must not
  // be spelled as MyInt like the other element.
  // CHECK-DAG: !DILocalVariable(name: "opaque",{{.*}} type: ![[OPAQUE:[0-9]+]]
  // CHECK-DAG: ![[OPAQUE]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[OPAQUE_TY:[0-9]+]])
  // CHECK-DAG: ![[OPAQUE_TY]] = !DICompositeType({{.*}}name: "$sSi_1a5MyIntatD"
  let opaque: (some P, MyInt) = (1, value)

  use(value); use(c); use(d); use(e); use(p); use(o); use(tup); use(mixed)
  use(m); use(mt); use(opaque)
}

// Specializing T with Int must not spell the first element as MyInt like the
// second one.
// OPT-DAG: ![[SPEC_FN:[0-9]+]] = distinct !DISubprogram(name: "specialized", linkageName: "$s1a11specializedyyx_SitlFSi_Tg5"
// OPT-DAG: ![[SPEC_SCOPE:[0-9]+]] = distinct !DILexicalBlock(scope: ![[SPEC_FN]]
// OPT-DAG: !DILocalVariable(name: "spec", scope: ![[SPEC_SCOPE]],{{.*}} type: ![[SPEC:[0-9]+]]
// OPT-DAG: ![[SPEC]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[SPEC_TY:[0-9]+]])
// OPT-DAG: ![[SPEC_TY]] = !DICompositeType({{.*}}name: "$sSi_1a5MyIntatD"
@inline(never)
func specialized<T>(_ x: T, _ y: MyInt) {
  let spec: (T, MyInt) = (x, y)
  use(spec)
}

public func callSpecialized(_ y: Int) { specialized(y, y) }

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

  // CHECK-DAG: !DILocalVariable(name: "weakRef",{{.*}} type: ![[WEAK:[0-9]+]]
  // CHECK-DAG: ![[WEAK]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[WEAK_TY:[0-9]+]])
  // CHECK-DAG: ![[WEAK_TY]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s1a3RefCyxAA5MyIntaGSgXwD"
  weak let weakRef: Ref<T, MyInt>? = nil

  // CHECK-DAG: !DICompositeType({{.*}}name: "$sSay1a5MyIntaxcGD"
  let fns: [(T) -> MyInt] = []
  use(gA); use(gC); use(phantom); use(weakRef); use(fns)
}

// Substitution can replace a pack expansion with any number of elements, so
// only the elements before the first and after the last one keep their sugar.
// CHECK-DAG: !DILocalVariable(name: "packTup",{{.*}} type: ![[PACK:[0-9]+]]
// CHECK-DAG: ![[PACK]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[PACK_TY:[0-9]+]])
// CHECK-DAG: ![[PACK_TY]] = !DICompositeType({{.*}}name: "$s1a5MyInta_xxQpAA6ToggleatD"
// The linkage name is matched by prefix, because function signature
// optimization appends to it.
// OPT-DAG: ![[PACK_FN:[0-9]+]] = distinct !DISubprogram(name: "packs", linkageName: "$s1a5packsyyxxQpRvzlFSi_SbQP_Tg5
// OPT-DAG: ![[PACK_SCOPE:[0-9]+]] = distinct !DILexicalBlock(scope: ![[PACK_FN]]
// OPT-DAG: !DILocalVariable(name: "packTup", scope: ![[PACK_SCOPE]],{{.*}} type: ![[OPT_PACK:[0-9]+]]
// OPT-DAG: ![[OPT_PACK]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[OPT_PACK_TY:[0-9]+]])
// OPT-DAG: ![[OPT_PACK_TY]] = !DICompositeType({{.*}}name: "$s1a5MyInta_SiSbAA6ToggleatD"
@inline(never)
func packs<each T>(_ x: repeat each T) {
  let packTup: (MyInt, repeat each T, Toggle) = (1, repeat each x, true)
  use(packTup)
}

public func callPacks() { packs(1, true) }

// The canonical types come first, so the sugared types are emitted as typedefs
// of them. The typealiases their names refer to must still be emitted.
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$s1a4ByteaD"
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$s1a5ShortaD"
public func canonicalFirst() {
  let plainTup: (Int8, Int8.Type) = (1, Int8.self)
  let plainMeta: Int16.Type = Int16.self
  let sugaredTup: (Byte, Byte.Type) = (1, Byte.self)
  let sugaredMeta: Short.Type = Short.self
  use(plainTup); use(plainMeta); use(sugaredTup); use(sugaredMeta)
}

// Generic arguments are emitted as forward declarations.
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$s1a4ElemaD"
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$s1a4KindaD"
public func genericArgs() {
  let boxedTup: Box<(Elem, Int)> = Box(t: (1, 2))
  let boxedMeta: Box<Kind.Type> = Box(t: Kind.self)
  use(boxedTup); use(boxedMeta)
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
