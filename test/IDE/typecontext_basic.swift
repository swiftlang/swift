// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=TEST_1 | %FileCheck %s -check-prefix=CHECK_1
// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=TEST_2 | %FileCheck %s -check-prefix=CHECK_1
// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=TEST_3 | %FileCheck %s -check-prefix=CHECK_1
// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=TEST_4 | %FileCheck %s -check-prefix=CHECK_1
// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=TEST_5 | %FileCheck %s -check-prefix=CHECK_1
// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=TEST_6 | %FileCheck %s -check-prefix=CHECK_2
// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=TEST_7 | %FileCheck %s -check-prefix=CHECK_2

// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=STMT_1 | %FileCheck %s -check-prefix=NO-CONTEXT
// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=SUFFIX_1 | %FileCheck %s -check-prefix=NO-CONTEXT
// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=SUFFIX_2 | %FileCheck %s -check-prefix=NO-CONTEXT
// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=SUFFIX_3 | %FileCheck %s -check-prefix=NO-CONTEXT
// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=SUFFIX_4 | %FileCheck %s -check-prefix=NO-CONTEXT
// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=DECL_1 | %FileCheck %s -check-prefix=NO-CONTEXT
// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=TYPE_1 | %FileCheck %s -check-prefix=NO-CONTEXT
// RUN: %target-swift-ide-test -type-context-info -source-filename %s -code-completion-token=TYPE_2 | %FileCheck %s -check-prefix=NO-CONTEXT

enum Direction {
  case east, west
  case unknown(String)
}
struct Target : OptionSet {
  /// Mine.
  static let me: Target = .init(rawValue: 1 << 0)
  /// Yours.
  static let you: Target = .init(rawValue: 1 << 1)
  /// Theirs.
  static let them: Target = .init(rawValue: 1 << 2)
  /// One for all.
  static var all: Target {
    return [.me, .you, .them]
  }
}

class C {
  func foo(x: Direction) {}
  func foo(x: Target) {}
}

func test1(obj: C) -> Direction {
  let _ = obj.foo(x: #^TEST_1^#
  let _ = obj.foo(x: #^TEST_2^#)
  let _ = obj.foo(x: #^TEST_3^#.east
  let _ = obj.foo(x: .#^TEST_4^#
  let _ = obj.foo(x: .#^TEST_5^#west

  let _: Direction = #^TEST_6^#;
  return #^TEST_7^#
}

// CHECK_1:      -----BEGIN TYPE CONTEXT INFO-----
// CHECK_1-NEXT: - TypeName: Direction
// CHECK_1-NEXT:   TypeUSR: $s14swift_ide_test9DirectionOD
// CHECK_1-NEXT:   ImplicitMembers:
// CHECK_1-NEXT:    - Name: east
// CHECK_1-NEXT:    - Name: west
// CHECK_1-NEXT:    - Name: unknown(_:)
// CHECK_1-NEXT: - TypeName: Target
// CHECK_1-NEXT:   TypeUSR: $s14swift_ide_test6TargetVD
// CHECK_1-NEXT:   ImplicitMembers:
// CHECK_1-NEXT:    - Name: me
// CHECK_1-NEXT:      DocBrief: "Mine."
// CHECK_1-NEXT:    - Name: you
// CHECK_1-NEXT:      DocBrief: "Yours."
// CHECK_1-NEXT:    - Name: them
// CHECK_1-NEXT:      DocBrief: "Theirs."
// CHECK_1-NEXT:    - Name: all
// CHECK_1-NEXT:      DocBrief: "One for all."
// CHECK_1-NEXT: -----END TYPE CONTEXT INFO-----

// CHECK_2:      -----BEGIN TYPE CONTEXT INFO-----
// CHECK_2-NEXT: - TypeName: Direction
// CHECK_2-NEXT:   TypeUSR: $s14swift_ide_test9DirectionOD
// CHECK_2-NEXT:   ImplicitMembers:
// CHECK_2-NEXT:    - Name: east
// CHECK_2-NEXT:    - Name: west
// CHECK_2-NEXT:    - Name: unknown(_:)
// CHECK_2-NEXT: -----END TYPE CONTEXT INFO-----

func test2(obj: C) {
  #^STMT_1^#

  let _ = obj #^SUFFIX_1^#
  let _ = obj .#^SUFFIX_2^#

  class Local1 : C {
    func test () {
      let _ = super #^SUFFIX_3^#
      let _ = super .#^SUFFIX_4^#
    }

    #^DECL_1^#
  }

  let _: #^TYPE_1^#
  class Local2 : #^TYPE_2^# {
  }
}

// NO-CONTEXT-NOT: TypeName
