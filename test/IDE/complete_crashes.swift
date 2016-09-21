// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BAD_MEMBERS_1 | %FileCheck %s -check-prefix=BAD_MEMBERS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BAD_MEMBERS_2 | %FileCheck %s -check-prefix=BAD_MEMBERS_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLOSURE_CALLED_IN_PLACE_1 | %FileCheck %s -check-prefix=WITH_GLOBAL

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

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LET_COMPUTED | %FileCheck %s -check-prefix=WITH_GLOBAL
class C {
  let x : Int { #^LET_COMPUTED^# }
}

// WITH_GLOBAL: Begin completions
// WITH_GLOBAL-DAG: Decl[FreeFunction]/CurrModule:      globalFunc()[#Void#]{{; name=.+$}}
// WITH_GLOBAL: End completions

({ x in 2+x })(#^CLOSURE_CALLED_IN_PLACE_1^#

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
  // GENERIC_PARAM_AND_ASSOC_TYPE-DAG: Decl[TypeAlias]/CurrNominal:        Value[#CustomGenericCollection.Value#]; name=Value
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
// FLIP_CURRIED: Pattern/ExprSpecific: ['(']{#Int#}, {#Int#})[#(Int) -> ()#]

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
