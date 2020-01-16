// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import Swift
import StdlibUnittest

let suite = TestSuite("Diffing")

// This availability test has to be this awkward because of
// rdar://problem/48450376 - Availability checks don't apply to top-level code
if #available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *) {

  suite.test("Diffing empty collections") {
    let a = [Int]()
    let b = [Int]()
    let diff = b.difference(from: a)
    expectEqual(diff, a.difference(from: a))
    expectTrue(diff.isEmpty)
  }

  suite.test("Basic diffing algorithm validators") {
    let expectedChanges: [(
        source: [String],
        target: [String],
        changes: [CollectionDifference<String>.Change],
        line: UInt
      )] = [
      (source:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        target:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        changes: [],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        target:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Presents",
          "New Years", "Champagne"],
        changes: [
          .remove(offset: 5, element: "Lights", associatedWith: nil)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        target:
        ["Hannukah", "Menorah", "Dreidel", "Gelt",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        changes: [
          .insert(offset: 3, element: "Gelt", associatedWith: nil)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        target:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Presents", "Tree", "Lights",
          "New Years", "Champagne"],
        changes: [
          .remove(offset: 6, element: "Presents", associatedWith: 4),
          .insert(offset: 4, element: "Presents", associatedWith: 6)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        target:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Lights", "Presents", "Tree",
          "New Years", "Champagne"],
        changes: [
          .remove(offset: 4, element: "Tree", associatedWith: 6),
          .insert(offset: 6, element: "Tree", associatedWith: 4)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        target:
        ["Hannukah", "Menorah", "Dreidel", "Presents",
          "Xmas", "Tree", "Lights",
          "New Years", "Champagne"],
        changes: [
          .remove(offset: 6, element: "Presents", associatedWith: 3),
          .insert(offset: 3, element: "Presents", associatedWith: 6)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        target:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights",
          "New Years", "Champagne", "Presents"],
        changes: [
          .remove(offset: 6, element: "Presents", associatedWith: 8),
          .insert(offset: 8, element: "Presents", associatedWith: 6)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        target:
        ["Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        changes: [
          .remove(offset: 2, element: "Dreidel", associatedWith: nil),
          .remove(offset: 1, element: "Menorah", associatedWith: nil),
          .remove(offset: 0, element: "Hannukah", associatedWith: nil)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents"],
        target:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        changes: [
          .insert(offset: 7, element: "New Years", associatedWith: nil),
          .insert(offset: 8, element: "Champagne", associatedWith: nil)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        target:
        ["New Years", "Champagne",
          "Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents"],
        changes: [
          .remove(offset: 8, element: "Champagne", associatedWith: 1),
          .remove(offset: 7, element: "New Years", associatedWith: 0),
          .insert(offset: 0, element: "New Years", associatedWith: 7),
          .insert(offset: 1, element: "Champagne", associatedWith: 8)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        target:
        ["Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne",
          "Hannukah", "Menorah", "Dreidel"],
        changes: [
          .remove(offset: 2, element: "Dreidel", associatedWith: 8),
          .remove(offset: 1, element: "Menorah", associatedWith: 7),
          .remove(offset: 0, element: "Hannukah", associatedWith: 6),
          .insert(offset: 6, element: "Hannukah", associatedWith: 0),
          .insert(offset: 7, element: "Menorah", associatedWith: 1),
          .insert(offset: 8, element: "Dreidel", associatedWith: 2)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel", "Presents",
          "Xmas", "Tree", "Lights",
          "New Years", "Champagne"],
        target:
        ["Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        changes: [
          .remove(offset: 3, element: "Presents", associatedWith: 3),
          .remove(offset: 2, element: "Dreidel", associatedWith: nil),
          .remove(offset: 1, element: "Menorah", associatedWith: nil),
          .remove(offset: 0, element: "Hannukah", associatedWith: nil),
          .insert(offset: 3, element: "Presents", associatedWith: 3)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents"],
        target:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Presents",
          "New Years", "Champagne", "Lights"],
        changes: [
          .remove(offset: 5, element: "Lights", associatedWith: 8),
          .insert(offset: 6, element: "New Years", associatedWith: nil),
          .insert(offset: 7, element: "Champagne", associatedWith: nil),
          .insert(offset: 8, element: "Lights", associatedWith: 5)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        target:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years"],
        changes: [
          .remove(offset: 8, element: "Champagne", associatedWith: nil)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel", "Presents",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne", "Presents"],
        target:
        ["Hannukah", "Menorah", "Dreidel", "Presents",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne", "Presents"],
        changes: [],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel", "Presents",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne", "Presents"],
        target:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights",
          "New Years", "Champagne", "Presents"],
        changes: [
          .remove(offset: 7, element: "Presents", associatedWith: nil),
          .remove(offset: 3, element: "Presents", associatedWith: nil)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights",
          "New Years", "Champagne", "Presents"],
        target:
        ["Hannukah", "Menorah", "Dreidel", "Presents",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne", "Presents"],
        changes: [
          .insert(offset: 3, element: "Presents", associatedWith: nil),
          .insert(offset: 7, element: "Presents", associatedWith: nil)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah", "Dreidel", "Presents",
          "Xmas", "Tree", "Lights",
          "New Years", "Champagne", "Presents"],
        target:
        ["Hannukah", "Menorah", "Dreidel",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne", "Presents"],
        changes: [
          .remove(offset: 3, element: "Presents", associatedWith: 6),
          .insert(offset: 6, element: "Presents", associatedWith: 3)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne",
          "Hannukah", "Dreidel"],
        target:
        ["Hannukah", "Menorah",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne",
          "Hannukah", "Dreidel"],
        changes: [],
        line: #line),

      (source:
        ["Hannukah", "Menorah",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne",
          "Hannukah", "Dreidel"],
        target:
        ["Hannukah", "Menorah",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        changes: [
          .remove(offset: 9, element: "Dreidel", associatedWith: nil),
          .remove(offset: 8, element: "Hannukah", associatedWith: nil)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne"],
        target:
        ["Hannukah", "Menorah",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne",
          "Hannukah", "Dreidel"],
        changes: [
          .insert(offset: 8, element: "Hannukah", associatedWith: nil),
          .insert(offset: 9, element: "Dreidel", associatedWith: nil)
        ],
        line: #line),

      (source:
        ["Hannukah", "Menorah",
          "Xmas", "Tree", "Lights", "Presents",
          "New Years", "Champagne",
          "Hannukah", "Dreidel"],
        target:
        ["Xmas", "Tree", "Lights", "Presents",
          "Hannukah", "Menorah",
          "New Years", "Champagne",
          "Hannukah", "Dreidel"],
        changes: [
          .remove(offset: 1, element: "Menorah", associatedWith: 5),
          .remove(offset: 0, element: "Hannukah", associatedWith: 4),
          .insert(offset: 4, element: "Hannukah", associatedWith: 0),
          .insert(offset: 5, element: "Menorah", associatedWith: 1)
        ],
        line: #line),
    ]

    for (source, target, expected, line) in expectedChanges {
      let actual = target.difference(from: source).inferringMoves()
      expectEqual(CollectionDifference(expected), actual, "failed test at line \(line)")
    }
  }

  suite.test("Empty diffs have sane behaviour") {
    guard let diff = CollectionDifference<String>([]) else {
      expectUnreachable()
      return
    }
    expectEqual(0, diff.insertions.count)
    expectEqual(0, diff.removals.count)
    expectEqual(true, diff.isEmpty)

    var c = 0
    diff.forEach({ _ in c += 1 })
    expectEqual(0, c)
  }

  suite.test("Happy path tests for the change validator") {
    // Base case: one insert and one remove with legal offsets
    expectNotNil(CollectionDifference<Int>.init([
          .insert(offset: 0, element: 0, associatedWith: nil),
          .remove(offset: 0, element: 0, associatedWith: nil)
        ]))

    // Code coverage:
    // • non-first change .remove has legal associated offset
    // • non-first change .insert has legal associated offset
    expectNotNil(CollectionDifference<Int>.init([
          .remove(offset: 1, element: 0, associatedWith: 0),
          .remove(offset: 0, element: 0, associatedWith: 1),
          .insert(offset: 0, element: 0, associatedWith: 1),
          .insert(offset: 1, element: 0, associatedWith: 0)
        ]))
  }

  suite.test("Exhaustive edge case tests for the change validator") {
    // Base case: two inserts sharing the same offset
    expectNil(CollectionDifference<Int>.init([
          .insert(offset: 0, element: 0, associatedWith: nil),
          .insert(offset: 0, element: 0, associatedWith: nil)
        ]))

    // Base case: two removes sharing the same offset
    expectNil(CollectionDifference<Int>.init([
          .remove(offset: 0, element: 0, associatedWith: nil),
          .remove(offset: 0, element: 0, associatedWith: nil)
        ]))

    // Base case: illegal insertion offset
    expectNil(CollectionDifference<Int>.init([
          .insert(offset: -1, element: 0, associatedWith: nil)
        ]))

    // Base case: illegal remove offset
    expectNil(CollectionDifference<Int>.init([
          .remove(offset: -1, element: 0, associatedWith: nil)
        ]))

    // Base case: two inserts sharing same associated offset
    expectNil(CollectionDifference<Int>.init([
          .insert(offset: 0, element: 0, associatedWith: 0),
          .insert(offset: 1, element: 0, associatedWith: 0)
        ]))

    // Base case: two removes sharing same associated offset
    expectNil(CollectionDifference<Int>.init([
          .remove(offset: 0, element: 0, associatedWith: 0),
          .remove(offset: 1, element: 0, associatedWith: 0)
        ]))

    // Base case: insert with illegal associated offset
    expectNil(CollectionDifference<Int>.init([
          .insert(offset: 0, element: 0, associatedWith: -1)
        ]))

    // Base case: remove with illegal associated offset
    expectNil(CollectionDifference<Int>.init([
          .remove(offset: 1, element: 0, associatedWith: -1)
        ]))

    // Code coverage: non-first change has illegal offset
    expectNil(CollectionDifference<Int>.init([
          .remove(offset: 0, element: 0, associatedWith: nil),
          .insert(offset: -1, element: 0, associatedWith: nil)
        ]))

    // Code coverage: non-first change has illegal associated offset
    expectNil(CollectionDifference<Int>.init([
          .remove(offset: 0, element: 0, associatedWith: nil),
          .insert(offset: 0, element: 0, associatedWith: -1)
        ]))
  }

  suite.test("Enumeration order is safe") {
    let safelyOrderedChanges: [CollectionDifference<Int>.Change] = [
      .remove(offset: 2, element: 0, associatedWith: nil),
      .remove(offset: 1, element: 0, associatedWith: 0),
      .remove(offset: 0, element: 0, associatedWith: 1),
      .insert(offset: 0, element: 0, associatedWith: 1),
      .insert(offset: 1, element: 0, associatedWith: 0),
      .insert(offset: 2, element: 0, associatedWith: nil),
    ]
    let diff = CollectionDifference<Int>.init(safelyOrderedChanges)!
    var enumerationOrderedChanges = [CollectionDifference<Int>.Change]()
    diff.forEach { c in
      enumerationOrderedChanges.append(c)
    }
    expectEqual(enumerationOrderedChanges, safelyOrderedChanges)
  }

  suite.test("Change validator rejects bad associations") {
    // .remove(1) → .insert(1)
    //   ↑      ↓
    // .insert(0) ← .remove(0)
    expectNil(CollectionDifference<Int>.init([
          .remove(offset: 1, element: 0, associatedWith: 1),
          .remove(offset: 0, element: 0, associatedWith: 0),
          .insert(offset: 0, element: 0, associatedWith: 1),
          .insert(offset: 1, element: 0, associatedWith: 0)
        ]))

    // Coverage: duplicate remove offsets both with assocs
    expectNil(CollectionDifference<Int>.init([
          .remove(offset: 0, element: 0, associatedWith: 1),
          .remove(offset: 0, element: 0, associatedWith: 0),
        ]))

    // Coverage: duplicate insert assocs
    expectNil(CollectionDifference<Int>.init([
          .insert(offset: 0, element: 0, associatedWith: 1),
          .insert(offset: 1, element: 0, associatedWith: 1),
        ]))
  }

  // Full-coverage test for CollectionDifference.Change.==()
  suite.test("Exhaustive testing for equatable conformance") {
    // Differs by type:
    expectNotEqual(
      CollectionDifference<Int>.Change.insert(offset: 0, element: 0, associatedWith: 0),
      CollectionDifference<Int>.Change.remove(offset: 0, element: 0, associatedWith: 0)
    )

    // Differs by type in the other direction:
    expectNotEqual(
      CollectionDifference<Int>.Change.remove(offset: 0, element: 0, associatedWith: 0),
      CollectionDifference<Int>.Change.insert(offset: 0, element: 0, associatedWith: 0)
    )

    // Insert differs by offset
    expectNotEqual(
      CollectionDifference<Int>.Change.insert(offset: 0, element: 0, associatedWith: 0),
      CollectionDifference<Int>.Change.insert(offset: 1, element: 0, associatedWith: 0)
    )

    // Insert differs by element
    expectNotEqual(
      CollectionDifference<Int>.Change.insert(offset: 0, element: 0, associatedWith: 0),
      CollectionDifference<Int>.Change.insert(offset: 0, element: 1, associatedWith: 0)
    )

    // Insert differs by association
    expectNotEqual(
      CollectionDifference<Int>.Change.insert(offset: 0, element: 0, associatedWith: 0),
      CollectionDifference<Int>.Change.insert(offset: 0, element: 0, associatedWith: 1)
    )

    // Remove differs by offset
    expectNotEqual(
      CollectionDifference<Int>.Change.remove(offset: 0, element: 0, associatedWith: 0),
      CollectionDifference<Int>.Change.remove(offset: 1, element: 0, associatedWith: 0)
    )

    // Remove differs by element
    expectNotEqual(
      CollectionDifference<Int>.Change.remove(offset: 0, element: 0, associatedWith: 0),
      CollectionDifference<Int>.Change.remove(offset: 0, element: 1, associatedWith: 0)
    )

    // Remove differs by association
    expectNotEqual(
      CollectionDifference<Int>.Change.remove(offset: 0, element: 0, associatedWith: 0),
      CollectionDifference<Int>.Change.remove(offset: 0, element: 0, associatedWith: 1)
    )
  }

  suite.test("Compile-time test of hashable conformance") {
    let _ = Set<CollectionDifference<String>>();
  }

  suite.test("Move inference") {
    let n = CollectionDifference<String>.init([
        .insert(offset: 3, element: "Sike", associatedWith: nil),
        .insert(offset: 4, element: "Sike", associatedWith: nil),
        .insert(offset: 2, element: "Hello", associatedWith: nil),
        .remove(offset: 6, element: "Hello", associatedWith: nil),
        .remove(offset: 8, element: "Goodbye", associatedWith: nil),
        .remove(offset: 9, element: "Sike", associatedWith: nil),
      ])
    let w = CollectionDifference<String>.init([
        .insert(offset: 3, element: "Sike", associatedWith: nil),
        .insert(offset: 4, element: "Sike", associatedWith: nil),
        .insert(offset: 2, element: "Hello", associatedWith: 6),
        .remove(offset: 6, element: "Hello", associatedWith: 2),
        .remove(offset: 8, element: "Goodbye", associatedWith: nil),
        .remove(offset: 9, element: "Sike", associatedWith: nil),
      ])
    expectEqual(w, n?.inferringMoves())
  }

  suite.test("Three way merge") {
    let baseLines = ["Is", "it", "time", "already?"]
    let theirLines = ["Hi", "there", "is", "it", "time", "already?"]
    let myLines = ["Is", "it", "review", "time", "already?"]

    // Create a difference from base to theirs
    let diff = theirLines.difference(from: baseLines)

    // Apply it to mine, if possible
    guard let patchedLines = myLines.applying(diff) else {
      print("Merge conflict applying patch, manual merge required")
      return
    }

    // Reassemble the result
    expectEqual(patchedLines, ["Hi", "there", "is", "it", "review", "time", "already?"])
    // print(patched)
  }

  suite.test("Diff reversal demo code") {
    let diff = CollectionDifference<Int>([])!
    let _ = CollectionDifference<Int>(
      diff.map({(change) -> CollectionDifference<Int>.Change in
          switch change {
          case .insert(offset: let o, element: let e, associatedWith: let a):
            return .remove(offset: o, element: e, associatedWith: a)
          case .remove(offset: let o, element: let e, associatedWith: let a):
            return .insert(offset: o, element: e, associatedWith: a)
          }
        })
    )!
  }

  suite.test("Naive application by enumeration") {
    var arr = ["Is", "it", "time", "already?"]
    let theirLines = ["Hi", "there", "is", "it", "time", "already?"]

    // Create a difference from base to theirs
    let diff = theirLines.difference(from: arr)

    for c in diff {
      switch c {
      case .remove(offset: let o, element: _, associatedWith: _):
        arr.remove(at: o)
      case .insert(offset: let o, element: let e, associatedWith: _):
        arr.insert(e, at: o)
      }
    }

    expectEqual(theirLines, arr)
  }

  suite.test("Fast applicator boundary conditions") {
    let a = [1, 2, 3, 4, 5, 6, 7, 8]
    for removeMiddle in [false, true] {
      for insertMiddle in [false, true] {
        for removeLast in [false, true] {
          for insertLast in [false, true] {
            for removeFirst in [false, true] {
              for insertFirst in [false, true] {
                var b = a

                // Prepare b
                if removeMiddle { b.remove(at: 4) }
                if insertMiddle { b.insert(10, at: 4) }
                if removeLast   { b.removeLast() }
                if insertLast   { b.append(11) }
                if removeFirst  { b.removeFirst() }
                if insertFirst  { b.insert(12, at: 0) }

                // Generate diff
                let diff = b.difference(from: a)

                // Validate application
                expectEqual(b, a.applying(diff)!)
                expectEqual(a, b.applying(diff.inverse())!)
              }}}}}}
  }

  suite.test("Fast applicator error condition") {
    let bear = "bear"
    let bird = "bird"
    let bat = "bat"

    let diff = bird.difference(from: bear)

    expectEqual(nil, bat.applying(diff))
  }

  suite.test("Fast applicator boundary condition remove last element") {
    let base = [1, 2, 3]

    expectEqual([1, 2], base.applying(CollectionDifference<Int>([.remove(offset: base.count - 1, element: 3, associatedWith: nil)])!))
  }

  suite.test("Fast applicator boundary condition append element") {
    let base = [1, 2, 3]

    expectEqual([1, 2, 3, 4], base.applying(CollectionDifference<Int>([.insert(offset: base.count, element: 4, associatedWith: nil)])!))
  }

  suite.test("Fast applicator error boundary condition remove at endIndex") {
    let base = [1, 2, 3]

    expectEqual(nil, base.applying(CollectionDifference<Int>([.remove(offset: base.count, element: 4, associatedWith: nil)])!))
  }

  suite.test("Fast applicator error boundary condition insert beyond end") {
    let base = [1, 2, 3]

    expectEqual(nil, base.applying(CollectionDifference<Int>([.insert(offset: base.count + 1, element: 5, associatedWith: nil)])!))
  }

  suite.test("Fast applicator boundary condition replace tail") {
    let base = [1, 2, 3]

    expectEqual([1, 2, 4], base.applying(CollectionDifference<Int>([
      .remove(offset: base.count - 1, element: 3, associatedWith: nil),
      .insert(offset: base.count - 1, element: 4, associatedWith: nil)
    ])!))
  }

  suite.test("Fast applicator error boundary condition insert beyond end after tail removal") {
    let base = [1, 2, 3]

    expectEqual(nil, base.applying(CollectionDifference<Int>([
      .remove(offset: base.count - 1, element: 3, associatedWith: nil),
      .insert(offset: base.count, element: 4, associatedWith: nil)
    ])!))

  }

  suite.test("Fast applicator error boundary condition insert beyond end after non-tail removal") {
    let base = [1, 2, 3]

    expectEqual(nil, base.applying(CollectionDifference<Int>([
      .remove(offset: base.count - 2, element: 2, associatedWith: nil),
      .insert(offset: base.count, element: 4, associatedWith: nil)
    ])!))
  }

  suite.test("Fast applicator fuzzer") {
    func makeArray() -> [Int] {
      var arr = [Int]()
      for _ in 0..<Int.random(in: 0..<10) {
        arr.append(Int.random(in: 0..<20))
      }
      return arr
    }
    for _ in 0..<1000 {
      let a = makeArray()
      let b = makeArray()
      let d = b.difference(from: a)
      let applied = a.applying(d)
      expectNotNil(applied)
      if let applied = applied {
        expectEqual(b, applied)
        expectEqual(a, applied.applying(d.inverse()))
        if (b != applied) {
          print("""
            // repro:
            let a = \(a)
            let b = \(b)
            let d = b.difference(from: a)
            expectEqual(b, a.applying(d))
            expectEqual(a, applied.applying(d.inverse()))
            """)
          break
        }
      }
    }
  }
}

runAllTests()
