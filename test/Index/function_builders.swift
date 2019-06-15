// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck -check-prefix=CHECK %s

struct Tagged<Tag, Entity> {
  let tag: Tag
  let entity: Entity
}

protocol Taggable {}
extension Taggable {
  func tag<Tag>(_ tag: Tag) -> Tagged<Tag, Self> {
    return Tagged(tag: tag, entity: self)
  }
}

extension Int: Taggable { }
extension String: Taggable { }
extension Double: Taggable { }

@_functionBuilder
struct TaggedBuilder<Tag> {
  static func buildBlock() -> () { }

  static func buildBlock<T1>(_ t1: Tagged<Tag, T1>) -> Tagged<Tag, T1> {
    return t1
  }

  static func buildBlock<T1, T2>(_ t1: Tagged<Tag, T1>, _ t2: Tagged<Tag, T2>) -> (Tagged<Tag, T1>, Tagged<Tag, T2>) {
    return (t1, t2)
  }

  static func buildIf<T>(_ value: Tagged<Tag, T>?) -> Tagged<Tag, T>? { return value }
}

enum Color {
  case red, green, blue
}

func acceptColorTagged<Result>(@TaggedBuilder<Color> body: () -> Result) {
// CHECK: [[@LINE-1]]:6 | function/Swift | acceptColorTagged(body:) | s:14swift_ide_test17acceptColorTagged4bodyyxyXE_tlF | Def | rel: 0
// CHECK: [[@LINE-2]]:33 | struct/Swift | TaggedBuilder | s:14swift_ide_test13TaggedBuilderV | Ref,RelCont | rel: 1
// CHECK: [[@LINE-3]]:47 | enum/Swift | Color | s:14swift_ide_test5ColorO | Ref,RelCont | rel: 1
  print(body())
}

struct Context {
    @TaggedBuilder<Color>
    // CHECK: [[@LINE-1]]:6 | struct/Swift | TaggedBuilder | s:14swift_ide_test13TaggedBuilderV | Ref | rel: 0
    // CHECK: [[@LINE-2]]:20 | enum/Swift | Color | s:14swift_ide_test5ColorO | Ref | rel: 0
    func foo() -> () {}
    // CHECK: [[@LINE-1]]:10 | instance-method/Swift | foo() | s:14swift_ide_test7ContextV3fooyyF | Def,RelChild | rel: 1
}
