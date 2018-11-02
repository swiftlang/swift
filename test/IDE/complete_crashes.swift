// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BAD_MEMBERS_1 | %FileCheck %s -check-prefix=BAD_MEMBERS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BAD_MEMBERS_2 | %FileCheck %s -check-prefix=BAD_MEMBERS_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLOSURE_CALLED_IN_PLACE_1 | %FileCheck %s -check-prefix=WITH_GLOBAL_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_28991372 | %FileCheck %s -check-prefix=RDAR_28991372

class BadMembers1 {
  var prop: Int {
    get {}
    a
  }
  subscript(i: Int) -> Double {
  }
}
func badMembers1(_ a: BadMembers1) {
  a#^BAD_MEMBERS_1^#
}
// BAD_MEMBERS_1: Begin completions
// BAD_MEMBERS_1-NEXT: Decl[InstanceVar]/CurrNominal: .prop[#Int#]{{; name=.+$}}
// BAD_MEMBERS_1-NEXT: Decl[Subscript]/CurrNominal:   [{#Int#}][#Double#]{{; name=.+$}}
// BAD_MEMBERS_1: End completions

protocol BadMembers2 {
  var prop: Int {
    get {}
    a
  }
  subscript(i: Int) -> Double {
  }
}
func badMembers2(_ a: BadMembers2) {
  a#^BAD_MEMBERS_2^#
}
// BAD_MEMBERS_2: Begin completions, 2 items
// BAD_MEMBERS_2-NEXT: Decl[InstanceVar]/CurrNominal: .prop[#Int#]{{; name=.+$}}
// BAD_MEMBERS_2-NEXT: Decl[Subscript]/CurrNominal:   [{#Int#}][#Double#]{{; name=.+$}}
// BAD_MEMBERS_2-NEXT: End completions

func globalFunc() {}

func globalFuncInt() -> Int { return 0 }

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LET_COMPUTED | %FileCheck %s -check-prefix=WITH_GLOBAL
class C {
  let x : Int { #^LET_COMPUTED^# }
}

// WITH_GLOBAL: Begin completions
// WITH_GLOBAL-DAG: Decl[FreeFunction]/CurrModule: globalFunc()[#Void#]; name=globalFunc()
// WITH_GLOBAL: End completions

({ x in 2+x })(#^CLOSURE_CALLED_IN_PLACE_1^#

// WITH_GLOBAL_INT: Begin completions
// WITH_GLOBAL_INT-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Identical]: globalFuncInt()[#Int#]; name=globalFuncInt()
// WITH_GLOBAL_INT: End completions

// rdar://19634354
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_19634354
while true {
  func f() {
    a#^RDAR_19634354^#
  }
}

// rdar://problem/21197042
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERIC_PARAM_AND_ASSOC_TYPE | %FileCheck %s -check-prefix=GENERIC_PARAM_AND_ASSOC_TYPE
struct CustomGenericCollection<Key> : ExpressibleByDictionaryLiteral {
  // GENERIC_PARAM_AND_ASSOC_TYPE: Begin completions
  // GENERIC_PARAM_AND_ASSOC_TYPE-DAG: Decl[InstanceVar]/CurrNominal:      count[#Int#]; name=count
  // GENERIC_PARAM_AND_ASSOC_TYPE-DAG: Decl[GenericTypeParam]/CurrNominal: Key[#Key#]; name=Key
  // GENERIC_PARAM_AND_ASSOC_TYPE-DAG: Decl[TypeAlias]/CurrNominal:        Value[#CustomGenericCollection<Key>.Value#]; name=Value
  // GENERIC_PARAM_AND_ASSOC_TYPE: End completions

  var count: Int { #^GENERIC_PARAM_AND_ASSOC_TYPE^# }
}

// rdar://problem/21796881
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_21796881
extension ExpressibleByNilLiteral {
   var nil: Self { #^RDAR_21796881^# }
}

// rdar://problem/21436558
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_21436558
private protocol RoundRobin : Sendable, Receivable {
  typealias _NEXT
}
#if TESTING
  extension RoundRobinAS {
    mutating func appendNextTo(_ acceptor:#^RDAR_21436558^#
  }
#endif

// rdar://problem/21435993
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_21435993
class C<T> {
  func test() {
    do {} catch { #^RDAR_21435993^# }
  }
  func accidentallyNested<U>(_ x: U) {}
}

// rdar://problem/21149908
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_21149908
@objc func handleTap(_ recognizer: UIGestureRecognizer) {
  if recognizer.state == .Ended {
    let _ : () = self.suggestion.cata(#^RDAR_21149908^#{ _ in
      listView?.backUpOneGroup()
      }, right: { _ in
        listView?.handleCompletion(self)
    }
  }
}

// rdar://problem/22036358
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_22036358
public extension AnyIterator {
  public extension AnySequence {
    public func take(_ n: Int) -> AnySequence<Element> {
      var xs: [Element] = []
      #^RDAR_22036358^#
      return AnySequence(xs)
    }
  }
}

// rdar://problem/22012123
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_22012123
protocol Fooable {
  protocol FooableZingable : Fooable {
    #^RDAR_22012123^#
  }
}

// rdar://problem/22688199
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_22688199 | %FileCheck %s -check-prefix=FLIP_CURRIED
func curried(_ a: Int)(_ b1: Int, _ b2: Int) { }
func flip<A, B, C>(_ f: A -> B -> C) -> B -> A -> C { }
func rdar22688199() {
  let f = flip(curried)(#^RDAR_22688199^#
}
// FLIP_CURRIED: Pattern/CurrModule: ['(']{#Int#}, {#Int#}[')'][#(Int) -> ()#]

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_22836263
func rdar22836263() {
  let x: [Int]
  nosuchfunc(x[0].#^RDAR_22836263^#)
}

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_22835966
func rdar22835966() {
  class Inner {
    var prop = #^RDAR_22835966^#
  }
}

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_22834017 | %FileCheck %s -check-prefix=INVALID_TYPE_INIT
struct Foo {
  let a: Anosuchtype
  let b: Bnosuchtype
  let c: Cnosuchtype
}

func rdar22834017() {
  Foo(#^RDAR_22834017^#)
}
// FIXME: We could provide a useful completion here. rdar://problem/22846558
// INVALID_TYPE_INIT-NOT: Begin completions

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_23173692 | %FileCheck %s -check-prefix=RDAR_23173692
func rdar23173692() {
  return IndexingIterator(#^RDAR_23173692^#)
}
// RDAR_23173692: Begin completions

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_22769393 | %FileCheck %s -check-prefix=RDAR_22769393
public enum PropertyListItem {
  case PLString(String)
  case PLDict([String:PropertyListItem])
}
class Connection {
  var handler: (Int32, [UInt8]) -> () = { _ in }
}
private let conn = Connection()
conn.handler = { (msgID, msg) in
  // Otherwise, we should have a structured message.
  let info = { () -> PropertyListItem in }()
  guard case .PLDict(var infoItems) = info else { fatalError("invalid message") }
  guard case .Some(.PLString(let command)) = infoItems["command"] else { fatalError("invalid message") }
  switch command {
  case "listSessions":
    var items = #^RDAR_22769393^#
  default:
    break
  }
}
// RDAR_22769393: Begin completions

struct S_RDAR_28991372 {
  init(x: Int, y: Int) {}
}

S_RDAR_28991372(x: #^RDAR_28991372^#, y: <#T##Int#>)
// RDAR_28991372: Begin completions

// rdar://problem/31981486
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_31981486 | %FileCheck %s -check-prefix=RDAR_31981486

protocol P where #^RDAR_31981486^#
// RDAR_31981486: Begin completions

// rdar://problem/38149042
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_38149042 | %FileCheck %s -check-prefix=RDAR_38149042
class Baz_38149042 {
  let x: Int = 0
}
protocol Bar_38149042 {
  var foo: Baz_38149042? {get}
}
func foo_38149042(bar: Bar_38149042) {
  _ = bar.foo? #^RDAR_38149042^# .x
}
// RDAR_38149042: Begin completions, 3 items
// RDAR_38149042-DAG: Decl[InstanceVar]/CurrNominal:                  .x[#Int#]; name=x
// RDAR_38149042-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]: [' ']=== {#AnyObject?#}[#Bool#]; name==== AnyObject?
// RDAR_38149042-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]: [' ']!== {#AnyObject?#}[#Bool#]; name=!== AnyObject?
// RDAR_38149042: End completions

// rdar://problem/38272904
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RDAR_38272904 | %FileCheck %s -check-prefix=RDAR_38272904
public protocol P_38272904 {
    associatedtype A = Error
    associatedtype B
}

public func ?? <T: P_38272904>(lhs: T, rhs: @autoclosure() throws -> T.B) rethrows -> T.B {
  fatalError()
}

class A_38272904 {
  open class func foo() -> A_38272904 { fatalError() }
}

func bar_38272904(a: A_38272904) {}
func foo_38272904(a: A_38272904) {
  bar_38272904(a: .foo() #^RDAR_38272904^#)
}
// RDAR_38272904: Begin completions
