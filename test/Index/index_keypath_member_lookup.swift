// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

struct Point {
// CHECK: [[@LINE-1]]:8 | struct/Swift | Point | {{.*}} | Def | rel: 0
  var x: Int
// CHECK: [[@LINE-1]]:7 | instance-property/Swift | x | [[PX_USR:s:.*]] | Def,RelChild | rel: 1
// CHECK: [[@LINE-2]]:7 | instance-method/acc-get/Swift | getter:x | [[PX_GET_USR:s:.*]] | Def,Impl,RelChild,RelAcc | rel: 1
// CHECK: [[@LINE-3]]:7 | instance-method/acc-set/Swift | setter:x | [[PX_SET_USR:s:.*]] | Def,Impl,RelChild,RelAcc | rel: 1
  var y: Int
// CHECK: [[@LINE-1]]:7 | instance-property/Swift | y | [[PY_USR:s:.*]] | Def,RelChild | rel: 1
// CHECK: [[@LINE-2]]:7 | instance-method/acc-get/Swift | getter:y | [[PY_GET_USR:s:.*]] | Def,Impl,RelChild,RelAcc | rel: 1
// CHECK: [[@LINE-3]]:7 | instance-method/acc-set/Swift | setter:y | [[PY_SET_USR:s:.*]] | Def,Impl,RelChild,RelAcc | rel: 1
}

struct Rectangle {
// CHECK: [[@LINE-1]]:8 | struct/Swift | Rectangle | {{.*}} | Def | rel: 0
  var topLeft: Point
// CHECK: [[@LINE-1]]:7 | instance-property/Swift | topLeft | [[TL_USR:s:.*]] | Def,RelChild | rel: 1
// CHECK: [[@LINE-2]]:7 | instance-method/acc-get/Swift | getter:topLeft | [[TL_GET_USR:s:.*]] | Def,Impl,RelChild,RelAcc | rel: 1
// CHECK: [[@LINE-3]]:7 | instance-method/acc-set/Swift | setter:topLeft | [[TL_SET_USR:s:.*]] | Def,Impl,RelChild,RelAcc | rel: 1
  var bottomRight: Point
// CHECK: [[@LINE-1]]:7 | instance-property/Swift | bottomRight | [[BR_USR:s:.*]] | Def,RelChild | rel: 1
// CHECK: [[@LINE-2]]:7 | instance-method/acc-get/Swift | getter:bottomRight | [[BR_GET_USR:s:.*]] | Def,Impl,RelChild,RelAcc | rel: 1
// CHECK: [[@LINE-3]]:7 | instance-method/acc-set/Swift | setter:bottomRight | [[BR_SET_USR:s:.*]] | Def,Impl,RelChild,RelAcc | rel: 1
}

@dynamicMemberLookup
struct Lens<T> {
// CHECK: [[@LINE-1]]:8 | struct/Swift | Lens | {{.*}} | Def | rel: 0
  var obj: T

  init(_ obj: T) {
    self.obj = obj
  }

  subscript<U>(dynamicMember member: WritableKeyPath<T, U>) -> Lens<U> {
// CHECK: [[@LINE-1]]:3 | instance-property/subscript/Swift | subscript(dynamicMember:) | [[SUB_USR:s:.*]] | Def,RelChild | rel: 1
    get { return Lens<U>(obj[keyPath: member]) }
// CHECK: [[@LINE-1]]:5 | instance-method/acc-get/Swift | getter:subscript(dynamicMember:) | [[SUB_GET_USR:s:.*]] | Def,RelChild,RelAcc | rel: 1
    set { obj[keyPath: member] = newValue.obj }
// CHECK: [[@LINE-1]]:5 | instance-method/acc-set/Swift | setter:subscript(dynamicMember:) | [[SUB_SET_USR:s:.*]] | Def,RelChild,RelAcc | rel: 1
  }
}


func testRead1(r: Lens<Rectangle>, a: Lens<[Int]>) {
  _ = r.topLeft
// CHECK: [[TL_LINE:[0-9]+]]:7 | param/Swift | r

// => implicit dynamicMember subscript (topLeft)
// CHECK: [[TL_LINE]]:8 | instance-property/subscript/Swift | subscript(dynamicMember:) | [[SUB_USR]] | Ref,Read,Impl,RelCont | rel: 1
// CHECK: [[TL_LINE]]:8 | instance-method/acc-get/Swift | getter:subscript(dynamicMember:) | [[SUB_GET_USR]] | Ref,Call,Impl,RelRec,RelCall,RelCont | rel: 2

// => property topLeft
// CHECK: [[TL_LINE]]:9 | instance-property/Swift | topLeft | [[TL_USR]] | Ref,Read,RelCont | rel: 1
// CHECK: [[TL_LINE]]:9 | instance-method/acc-get/Swift | getter:topLeft | [[TL_GET_USR]] | Ref,Call,Impl,RelCall,RelCont | rel: 1

  _ = r.bottomRight.y
// CHECK: [[BR_LINE:[0-9]+]]:7 | param/Swift | r

// => implicit dynamicMember subscript (bottomRight)
// CHECK: [[BR_LINE]]:8 | instance-property/subscript/Swift | subscript(dynamicMember:) | [[SUB_USR]] | Ref,Read,Impl,RelCont | rel: 1
// CHECK: [[BR_LINE]]:8 | instance-method/acc-get/Swift | getter:subscript(dynamicMember:) | [[SUB_GET_USR]] | Ref,Call,Impl,RelRec,RelCall,RelCont | rel: 2

// => property bottomRight
// CHECK: [[BR_LINE]]:9 | instance-property/Swift | bottomRight | [[BR_USR]] | Ref,Read,RelCont | rel: 1
// CHECK: [[BR_LINE]]:9 | instance-method/acc-get/Swift | getter:bottomRight | [[BR_GET_USR]] | Ref,Call,Impl,RelCall,RelCont | rel: 1

// => implicit dynamicMember subscript (y)
// CHECK: [[BR_LINE]]:20 | instance-property/subscript/Swift | subscript(dynamicMember:) | [[SUB_USR]] | Ref,Read,Impl,RelCont | rel: 1
// CHECK: [[BR_LINE]]:20 | instance-method/acc-get/Swift | getter:subscript(dynamicMember:) | [[SUB_GET_USR]] | Ref,Call,Impl,RelRec,RelCall,RelCont | rel: 2

// => property y
// CHECK: [[BR_LINE]]:21 | instance-property/Swift | y | [[PY_USR]] | Ref,Read,RelCont | rel: 1
// CHECK: [[BR_LINE]]:21 | instance-method/acc-get/Swift | getter:y | [[PY_GET_USR]] | Ref,Call,Impl,RelCall,RelCont | rel: 1

  _ = a[0]
// CHECK: [[A_LINE:[0-9]+]]:7 | param/Swift | a

// => implicit dynamicMember subscript
// CHECK: [[A_LINE]]:8 | instance-property/subscript/Swift | subscript(dynamicMember:) | [[SUB_USR]] | Ref,Read,Impl,RelCont | rel: 1
// CHECK: [[A_LINE]]:8 | instance-method/acc-get/Swift | getter:subscript(dynamicMember:) | [[SUB_GET_USR]] | Ref,Call,Impl,RelRec,RelCall,RelCont | rel: 2

// => subscript [Int]
// CHECK: [[A_LINE]]:8 | instance-property/subscript/Swift | subscript(_:) | s:SayxSicip | Ref,Read,RelCont | rel: 1
// CHECK: [[A_LINE]]:8 | instance-method/acc-get/Swift | getter:subscript(_:) | s:SayxSicig | Ref,Call,Impl,RelCall,RelCont | rel: 1
}


func testWrite1(r: inout Lens<Rectangle>, p: Lens<Point>, a: inout Lens<[Int]>) {
  r.topLeft = p
// CHECK: [[WTL_LINE:[0-9]+]]:3 | param/Swift | r

// => implicit dynamicMember subscript (topLeft)
// CHECK: [[WTL_LINE]]:4 | instance-property/subscript/Swift | subscript(dynamicMember:) | [[SUB_USR]] | Ref,Writ,Impl,RelCont | rel: 1
// CHECK: [[WTL_LINE]]:4 | instance-method/acc-set/Swift | setter:subscript(dynamicMember:) | [[SUB_SET_USR]] | Ref,Call,Impl,RelCall,RelCont | rel: 1

// => property topLeft
// CHECK: [[WTL_LINE]]:5 | instance-property/Swift | topLeft | [[TL_USR]] | Ref,Writ,RelCont | rel: 1
// CHECK: [[WTL_LINE]]:5 | instance-method/acc-set/Swift | setter:topLeft | [[TL_SET_USR]] | Ref,Call,Impl,RelCall,RelCont | rel: 1

  r.bottomRight.y = Lens(3)
// CHECK: [[WBR_LINE:[0-9]+]]:3 | param/Swift | r

// => implicit dynamicMember subscript (bottomRight)
// CHECK: [[WBR_LINE:[0-9]+]]:4 | instance-property/subscript/Swift | subscript(dynamicMember:) | [[SUB_USR]] | Ref,Read,Writ,Impl,RelCont | rel: 1
// CHECK: [[WBR_LINE:[0-9]+]]:4 | instance-method/acc-get/Swift | getter:subscript(dynamicMember:) | [[SUB_GET_USR]] | Ref,Call,Impl,RelCall,RelCont | rel: 1
// CHECK: [[WBR_LINE:[0-9]+]]:4 | instance-method/acc-set/Swift | setter:subscript(dynamicMember:) | [[SUB_SET_USR]] | Ref,Call,Impl,RelCall,RelCont | rel: 1

// => property bottomRight
// CHECK: [[WBR_LINE:[0-9]+]]:5 | instance-property/Swift | bottomRight | [[BR_USR]] | Ref,Read,Writ,RelCont | rel: 1
// CHECK: [[WBR_LINE:[0-9]+]]:5 | instance-method/acc-get/Swift | getter:bottomRight | [[BR_GET_USR]] | Ref,Call,Impl,RelCall,RelCont | rel: 1
// CHECK: [[WBR_LINE:[0-9]+]]:5 | instance-method/acc-set/Swift | setter:bottomRight | [[BR_SET_USR]] | Ref,Call,Impl,RelCall,RelCont | rel: 1

// => implicit dynamicMember subscript (y)
// CHECK: [[WBR_LINE:[0-9]+]]:16 | instance-property/subscript/Swift | subscript(dynamicMember:) | [[SUB_USR]] | Ref,Writ,Impl,RelCont | rel: 1
// CHECK: [[WBR_LINE:[0-9]+]]:16 | instance-method/acc-set/Swift | setter:subscript(dynamicMember:) | [[SUB_SET_USR]] | Ref,Call,Impl,RelCall,RelCont | rel: 1

// => property y
// CHECK: [[WBR_LINE:[0-9]+]]:17 | instance-property/Swift | y | [[PY_USR]] | Ref,Writ,RelCont | rel: 1
// CHECK: [[WBR_LINE:[0-9]+]]:17 | instance-method/acc-set/Swift | setter:y | [[PY_SET_USR]] | Ref,Call,Impl,RelCall,RelCont | rel: 1

  // FIXME: crashes typechecker rdar://problem/49533404
  // a[0] = Lens(1)
}

func testExplicit(r: Lens<Rectangle>, a: Lens<[Int]>) {
  _ = r[dynamicMember: \.topLeft]
// CHECK: [[ETL_LINE:[0-9]+]]:7 | param/Swift | r
// Not implicit.
// CHECK: [[ETL_LINE]]:8 | instance-property/subscript/Swift | subscript(dynamicMember:) | [[SUB_USR]] | Ref,Read,RelCont | rel: 1
// CHECK: [[ETL_LINE]]:26 | instance-property/Swift | topLeft | [[TL_USR]] | Ref,Read,RelCont | rel: 1

  _ = a[dynamicMember: \.[0]]
// CHECK: [[EA_LINE:[0-9]+]]:7 | param/Swift | a
// Not implicit.
// CHECK: [[EA_LINE]]:8 | instance-property/subscript/Swift | subscript(dynamicMember:) | [[SUB_USR]] | Ref,Read,RelCont | rel: 1
// CHECK: [[EA_LINE]]:26 | instance-property/subscript/Swift | subscript(_:) | s:SayxSicip | Ref,Read,RelCont | rel: 1
}

// Don't crash: rdar63558609
//
@dynamicMemberLookup
protocol Foo {
  var prop: Bar {get}
  // CHECK: [[@LINE-1]]:7 | instance-property/Swift | prop | [[PROP_USR:.*]] | Def,RelChild | rel: 1
}
struct Bar {
  let enabled = false
}
extension Foo {
  subscript<T>(dynamicMember keyPath: KeyPath<Bar,T>) -> T {
  // CHECK: [[@LINE-1]]:3 | instance-property/subscript/Swift | subscript(dynamicMember:) | [[SUB2_USR:.*]] | Def,RelChild | rel: 1
  // CHECK: [[@LINE-2]]:60 | instance-method/acc-get/Swift | getter:subscript(dynamicMember:) | {{.*}} | Def,Dyn,RelChild,RelAcc | rel: 1
  // CHECK-NEXT: RelChild,RelAcc | instance-property/subscript/Swift | subscript(dynamicMember:) | [[SUB2_USR]]

    prop[keyPath: keyPath]
    // CHECK: [[@LINE-1]]:5 | instance-property/Swift | prop | [[PROP_USR]] | Ref,Read,RelCont | rel: 1
  }
}
