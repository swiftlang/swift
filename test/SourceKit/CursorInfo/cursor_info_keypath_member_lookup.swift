struct Point {
  var x: Int
  var y: Int
}

struct Rectangle {
  var topLeft: Point
  var bottomRight: Point
}

@dynamicMemberLookup
struct Lens<T> {
  var obj: T
  init(_ obj: T) {
    self.obj = obj
  }

  subscript<U>(dynamicMember member: WritableKeyPath<T, U>) -> Lens<U> {
    get { return Lens<U>(obj[keyPath: member]) }
    set { obj[keyPath: member] = newValue.obj }
  }
}

func test(r: Lens<Rectangle>) {
  _ = r.topLeft
  _ = r.bottomRight.y
}

// RUN: %sourcekitd-test -req=cursor -pos=18:3 -cursor-action %s -- %s | %FileCheck -check-prefix=SUBSCRIPT %s
// SUBSCRIPT: source.lang.swift.decl.function.subscript (18:3-18:12)
// SUBSCRIPT: subscript(dynamicMember:)
// SUBSCRIPT: <T, U> (dynamicMember: WritableKeyPath<T, U>) -> Lens<U>
// SUBSCRIPT: ACTIONS BEGIN
// FIXME: should not allow rename since that would break the contract.
// SUBSCRIPT: source.refactoring.kind.rename.global
// SUBSCRIPT: ACTIONS END

// RUN: %sourcekitd-test -req=cursor -pos=25:9 -cursor-action %s -- %s | %FileCheck -check-prefix=TOP_LEFT %s
// TOP_LEFT: source.lang.swift.ref.var.instance
// TOP_LEFT: topLeft
// TOP_LEFT: Point
// TOP_LEFT: <decl.var.instance><syntaxtype.keyword>var</syntaxtype.keyword> <decl.name>topLeft</decl.name>: <decl.var.type><ref.struct usr="{{s:.*}}">Point</ref.struct></decl.var.type></decl.var.instance>
// TOP_LEVEL: ACTIONS BEGIN
// TOP_LEVEL: source.refactoring.kind.rename.global
// TOP_LEVEL: ACTIONS END

// RUN: %sourcekitd-test -req=cursor -pos=26:9 -cursor-action %s -- %s | %FileCheck -check-prefix=BOTTOM_RIGHT %s
// BOTTOM_RIGHT: source.lang.swift.ref.var.instance
// BOTTOM_RIGHT: bottomRight
// BOTTOM_RIGHT: Point
// BOTTOM_RIGHT: ACTIONS BEGIN
// BOTTOM_RIGHT: source.refactoring.kind.rename.global
// BOTTOM_RIGHT: ACTIONS END

/// RUN: %sourcekitd-test -req=cursor -pos=26:21 -cursor-action %s -- %s | %FileCheck -check-prefix=YREF %s
// YREF: source.lang.swift.ref.var.instance
// YREF: y
// YREF: Int
// YREF: <decl.var.instance><syntaxtype.keyword>var</syntaxtype.keyword> <decl.name>y</decl.name>: <decl.var.type><ref.struct usr="s:Si">Int</ref.struct></decl.var.type></decl.var.instance>
// YREF: ACTIONS BEGIN
// YREF: source.refactoring.kind.rename.global
// YREF: ACTIONS END
