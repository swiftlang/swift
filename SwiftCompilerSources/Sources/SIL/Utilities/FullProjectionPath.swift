//===--- FullProjectionPath.swift - an unbounded path of projections ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Basic

/// A projection path of arbitrary depth.
///
/// `FullProjectionPath` is a faithful generalization of `SmallProjectionPath`: it has the same
/// syntax, the same `FieldKind` vocabulary (including the `**`, `v**`, `c*`, `i*` wildcards), and
/// the same `push`/`pop`/`popIfMatches`/`merge` semantics. The only difference is the backing
/// store: instead of packing components into a single 64-bit word (which saturates to `**` past
/// ~8 components), it stores them in an array. A concrete path therefore *never* loses precision,
/// no matter how deeply nested the projection is.
///
/// This matters for a field-sensitive analysis: a `SmallProjectionPath` that saturated to `**` on a
/// deeply-nested access would over-approximate the touched leaves, which could turn a legal
/// consume into a spurious "used after consume" diagnostic. `FullProjectionPath` avoids that.
///
/// Like `SmallProjectionPath`, it conforms to `WalkingPath`, so it can be used as the `Path` of an
/// `AddressDefUseWalker` (or any other walker) as a drop-in replacement.
///
/// A future optimization could make this an enum that stays a `SmallProjectionPath` until it would
/// saturate and only then promotes to the array form; today we always use the array for simplicity.
public struct FullProjectionPath : WalkingPath, Hashable, CustomStringConvertible {
  public typealias FieldKind = SmallProjectionPath.FieldKind

  /// A single projection: a `FieldKind` plus (for "small" kinds) a field index.
  /// Wildcard/"large" kinds carry an index of `0`, mirroring `SmallProjectionPath`.
  public struct Component : Hashable {
    public let kind: FieldKind
    public let index: Int

    public init(_ kind: FieldKind, _ index: Int) {
      self.kind = kind
      self.index = index
    }
  }

  /// The path components, stored top-*last*: `components.last` is the top (root-nearest)
  /// component, i.e. the next one to `pop`; `components[0]` is the deepest component. Storing the
  /// top at the end of the array keeps `push`/`pop` on the array's cheap end (`append`/`dropLast`)
  /// rather than the expensive front (`insert(at: 0)`/`dropFirst`) -- mirroring how
  /// `SmallProjectionPath` keeps the top in the low bits. An empty array is the "root" (empty)
  /// path.
  ///
  /// Every initializer normalizes to this order, so two logically-equal paths always have
  /// identical storage; that is what makes the derived `Equatable`/`Hashable` correct.
  private var components: [Component]

  /// Builds a path from components already in internal storage order (top-last).
  private init(storage: [Component]) {
    self.components = storage
  }

  /// Builds a path from a list of components given top-*first* (root-nearest first) order.
  /// Normalizes to the internal top-last storage.
  init(components topFirst: [Component]) {
    self.components = Array(topFirst.reversed())
  }

  public init() {
    self.components = []
  }

  /// Creates a new path with an initial (and only) component.
  public init(_ kind: FieldKind, index: Int = 0) {
    self = Self().push(kind, index: index)
  }

  /// Converts a `SmallProjectionPath` into a `FullProjectionPath` with identical components.
  /// Handy for interop and for tests, which can reuse `parseProjectionPathFromSIL`.
  public init(from small: SmallProjectionPath) {
    var topFirst: [Component] = []
    var p = small
    while true {
      let (kind, index, rest) = p.pop()
      if kind == .root { break }
      topFirst.append(Component(kind, index))
      p = rest
    }
    self.init(components: topFirst)
  }

  public var isEmpty: Bool { components.isEmpty }

  /// The kind of the top component, or `.root` if the path is empty.
  private var topKind: FieldKind { components.last?.kind ?? .root }

  /// Returns the path with its top component removed. Precondition: not empty.
  private func removingTop() -> Self {
    Self(storage: Array(components.dropLast()))
  }

  public var description: String {
    // Storage is top-last; print top-first ("s0.s1.e2").
    return components.reversed().map { component in
      switch component.kind {
      case .root:              return ""
      case .structField:       return "s\(component.index)"
      case .tupleField:        return "\(component.index)"
      case .enumCase:          return "e\(component.index)"
      case .classField:        return "c\(component.index)"
      case .tailElements:      return "ct"
      case .existential:       return "x"
      case .vectorBase:        return "b"
      case .indexedElement:    return "i\(component.index)"
      case .anyIndexedElement: return "i*"
      case .anything:          return "**"
      case .anyValueFields:    return "v**"
      case .anyClassField:     return "c*"
      }
    }.joined(separator: ".")
  }

  /// Pops and returns the top (root-nearest) path component together with the remaining path.
  ///
  /// For an empty path this returns `(.root, 0, self)`, matching `SmallProjectionPath`.
  public func pop() -> (kind: FieldKind, index: Int, path: FullProjectionPath) {
    guard let top = components.last else {
      return (.root, 0, self)
    }
    return (top.kind, top.index, removingTop())
  }

  /// Pushes a new top (root-nearest) component and returns the new path.
  ///
  /// For example, pushing `s0` onto `c3.e1` yields `s0.c3.e1`.
  public func push(_ kind: FieldKind, index: Int = 0) -> FullProjectionPath {
    assert(kind != .anything || isEmpty, "'anything' only allowed in last path component")
    var storage = components
    storage.append(Component(kind, index))
    return Self(storage: storage)
  }

  /// Pops the top component if it is exactly of kind `kind`.
  public func pop(kind: FieldKind) -> (index: Int, path: FullProjectionPath)? {
    let (k, idx, newPath) = pop()
    if k != kind { return nil }
    return (idx, newPath)
  }

  /// Pops the top component if it matches `kind` and (optionally) `index`.
  ///
  /// Wildcards behave exactly as in `SmallProjectionPath`:
  ///   popping `s0` from `v**.c3.e1` returns `v**.c3.e1` (any number of value fields)
  ///   popping `c2` from `c*.e1`     returns `e1`
  ///   popping `s0` from `c*.e1`     returns nil
  public func popIfMatches(_ kind: FieldKind, index: Int? = nil) -> FullProjectionPath? {
    let (k, idx, _) = pop()
    switch k {
    case .anything:
      return self
    case .anyValueFields:
      if kind.isValueField { return self }
      return removingTop().popIfMatches(kind, index: index)
    case .anyClassField:
      if kind.isClassField { return removingTop() }
      return nil
    case .anyIndexedElement:
      if kind.isIndexedElement { return removingTop() }
      return nil
    case kind:
      if let i = index, i != idx { return nil }
      return removingTop()
    default:
      return nil
    }
  }

  /// Pops all leading value-field components. E.g. `s0.e2.3.c4.s1` -> `c4.s1`.
  public func popAllValueFields() -> FullProjectionPath {
    var p = self
    while p.topKind.isValueField {
      p = p.removingTop()
    }
    return p
  }

  /// Returns the merged path of this path and `rhs`: the least-general path that both match.
  /// Mismatching components are replaced by wildcards, exactly as in `SmallProjectionPath.merge`.
  ///   `s0.c3.e4` merged with `s0.c1.e4` -> `s0.c*.e4`
  ///   `s0.s1.c3` merged with `e4.c3`    -> `v**.c3`
  ///   `s0.c1.c2` merged with `s0.c3`    -> `s0.**`
  public func merge(with rhs: FullProjectionPath) -> FullProjectionPath {
    if self == rhs { return self }

    let (lhsKind, lhsIdx, _) = pop()
    let (rhsKind, rhsIdx, _) = rhs.pop()

    if lhsKind == rhsKind && lhsIdx == rhsIdx {
      let subPath = removingTop().merge(with: rhs.removingTop())
      if lhsKind == .anyValueFields && subPath.topKind == .anyValueFields {
        return subPath
      }
      return subPath.push(lhsKind, index: lhsIdx)
    }
    if lhsKind.isIndexedElement && rhsKind.isIndexedElement {
      let subPath = removingTop().merge(with: rhs.removingTop())
      return subPath.push(.anyIndexedElement)
    }
    if lhsKind.isValueField || rhsKind.isValueField {
      let subPath = popAllValueFields().merge(with: rhs.popAllValueFields())
      assert(!subPath.topKind.isValueField)
      if subPath.topKind == .anything {
        return subPath
      }
      return subPath.push(.anyValueFields)
    }
    if lhsKind.isClassField && rhsKind.isClassField {
      let subPath = removingTop().merge(with: rhs.removingTop())
      return subPath.push(.anyClassField)
    }
    return Self(.anything)
  }
}

//===----------------------------------------------------------------------===//
//                               Unit Tests
//===----------------------------------------------------------------------===//

let fullProjectionPathTest = Test("full_projection_path") {
  function, arguments, context in

  basicPushPop()
  smallPathParity()
  deepPathStaysPrecise()
  popIfMatchesCases()
  merging()

  // Build a `FullProjectionPath` from SIL syntax by reusing `SmallProjectionPath`'s parser.
  func parse(_ str: String) -> FullProjectionPath {
    var parser = StringParser(str)
    return FullProjectionPath(from: try! parser.parseProjectionPathFromSIL())
  }

  func basicPushPop() {
    let p1 = FullProjectionPath(.structField, index: 3)
                       .push(.classField, index: 12345678)
    let (k2, i2, p2) = p1.pop()
    assert(k2 == .classField && i2 == 12345678)
    let (k3, i3, p3) = p2.pop()
    assert(k3 == .structField && i3 == 3)
    assert(p3.isEmpty)

    // pop() on an empty path yields the `.root` sentinel.
    let (kr, ir, pr) = FullProjectionPath().pop()
    assert(kr == .root && ir == 0 && pr.isEmpty)

    // pop(kind:) only succeeds on an exact match.
    assert(p1.pop(kind: .structField) == nil)
    let (idx, _) = p1.pop(kind: .classField)!
    assert(idx == 12345678)
  }

  // On paths that fit in a `SmallProjectionPath`, the two representations agree on description.
  func smallPathParity() {
    for str in ["s0", "s3.c12.e1", "2.c*.e6.ct.**", "v**.c*", "i3.x.b.i*", "s0.v**.s2"] {
      var parser = StringParser(str)
      let small = try! parser.parseProjectionPathFromSIL()
      let full = FullProjectionPath(from: small)
      assert(full.description == small.description)
      assert(full.description == str)
    }
  }

  // A path far deeper than a `SmallProjectionPath` can hold stays fully precise, whereas the
  // equivalent `SmallProjectionPath` saturates to `**`.
  func deepPathStaysPrecise() {
    let depth = 40

    // SmallProjectionPath cannot hold this many components: pushed one at a time, it saturates to
    // `**` well before reaching `depth`.
    var small = SmallProjectionPath()
    var saturated = false
    for i in (0..<depth).reversed() {
      small = small.push(.structField, index: i)
      if small == SmallProjectionPath(.anything) {
        saturated = true
        break
      }
    }
    assert(saturated)

    // The full path retains every component, in order.
    var full = FullProjectionPath()
    for i in (0..<depth).reversed() {
      full = full.push(.structField, index: i)
    }
    var p = full
    for i in 0..<depth {
      let (k, idx, rest) = p.pop()
      assert(k == .structField && idx == i)
      p = rest
    }
    assert(p.isEmpty)
  }

  func popIfMatchesCases() {
    assert(parse("v**.c3").popIfMatches(.anyValueFields)!.description == "v**.c3")
    assert(parse("**").popIfMatches(.anyValueFields)!.description == "**")
    assert(parse("s0.c3").popIfMatches(.anyValueFields) == nil)
    assert(parse("c0.s3").popIfMatches(.anyClassField) == nil)
    assert(parse("c*.e3").popIfMatches(.anyClassField)!.description == "e3")
    assert(parse("s0.s1").popIfMatches(.structField, index: 0)!.description == "s1")
    assert(parse("s0.s1").popIfMatches(.structField, index: 1) == nil)
    // `v**` absorbs any number of value fields.
    assert(parse("v**.c1").popIfMatches(.structField, index: 7)!.description == "v**.c1")
  }

  func merging() {
    testMerge("c1.c0",    "c0",     expect: "c*.**")
    testMerge("c2.c1",    "c2",     expect: "c2.**")
    testMerge("s3.c0",    "v**.c0", expect: "v**.c0")
    testMerge("c0",       "s2.c1",  expect: "v**.c*")
    testMerge("s1.s1.c2", "s1.c2",  expect: "s1.v**.c2")
    testMerge("s1.s0",    "s2.s0",  expect: "v**")
    testMerge("s0.s1",    "s0.s1",  expect: "s0.s1")
  }

  func testMerge(_ lhsStr: String, _ rhsStr: String, expect expectStr: String) {
    let lhs = parse(lhsStr)
    let rhs = parse(rhsStr)
    let expect = parse(expectStr)
    assert(lhs.merge(with: rhs) == expect)
    assert(rhs.merge(with: lhs) == expect)
  }
}
