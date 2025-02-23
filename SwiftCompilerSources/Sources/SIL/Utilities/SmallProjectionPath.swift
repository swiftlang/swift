//===--- SmallProjectionPath.swift - a path of projections ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Basic

// Needed to make some important utility functions in `Basic` available to importers of `SIL`.
// For example, let `Basic.assert` be used in Optimizer module.
@_exported import Basic

/// A small and very efficient representation of a projection path.
///
/// A `SmallProjectionPath` can be parsed and printed in SIL syntax and parsed from Swift
/// source code - the SIL syntax is more compact than the Swift syntax.
/// In the following, we use the SIL syntax for the examples.
///
/// The `SmallProjectionPath` represents a path of value or address projections.
/// For example, the projection path which represents
///
///   %t = struct_extract %s: $Str, #Str.tupleField    // first field in Str
///   %c = tuple_extract %f : $(Int, Class), 4
///   %r = ref_element_addr %c $Class, #Class.classField // 3rd field in Class
///
/// is `s0.4.c2`, where `s0` is the first path component and `c2` is the last component.
///
/// A `SmallProjectionPath` can be a concrete path (like the example above): it only
/// contains concrete field elements, e.g. `s0.c2.e1`
/// Or it can be a pattern path, where one or more path components are wild cards, e.g.
/// `v**.c*` means: any number of value projections (struct, enum, tuple) followed by
/// a single class field projection.
///
/// Internally, a `SmallProjectionPath` is represented as a single 64-bit word.
/// This is very efficient, but it also means that a path cannot exceed a certain length.
/// If too many projections are pushed onto a path, the path is converted to a `**` wildcard,
/// which means: it represents any number of any kind of projections.
/// Though, it's very unlikely that the limit will be reached in real world scenarios.
///
public struct SmallProjectionPath : Hashable, CustomStringConvertible, NoReflectionChildren {

  /// The physical representation of the path. The path components are stored in
  /// reverse order: the first path component is stored in the lowest bits (LSB),
  /// the last component is stored in the highest bits (MSB).
  /// Each path component consists of zero or more "index-overflow" bytes followed
  /// by the "index-kind" main byte (from LSB to MSB).
  ///
  /// index overflow byte:    bit-nr:  | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
  ///                                  +---+---+---+---+---+---+---+---+
  ///                         content: |      index high bits      | 1 |
  ///
  /// main byte (small kind): bit-nr:  | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
  ///                                  +---+---+---+---+---+---+---+---+
  ///                         content: | index low bits|   kind    | 0 |
  ///
  /// "Large" kind values (>= 0x7) don't have an associated index and the main
  /// byte looks like:
  ///
  /// main byte (large kind): bit-nr:  | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
  ///                                  +---+---+---+---+---+---+---+---+
  ///                         content: | kind high bits| 1 | 1 | 1 | 0 |
  ///
  private let bytes: UInt64

  // TODO: add better support for tail elements by tracking the
  // index of `index_addr` instructions.
  public enum FieldKind : Int {
    case root           = 0x0 // The pseudo component denoting the end of the path.
    case structField    = 0x1 // A concrete struct field: syntax e.g. `s3`
    case tupleField     = 0x2 // A concrete tuple element: syntax e.g. `2`
    case enumCase       = 0x3 // A concrete enum case (with payload): syntax e.g. `e4'
    case classField     = 0x4 // A concrete class field: syntax e.g. `c1`
    case indexedElement = 0x5 // A constant offset into an array of elements: syntax e.g. 'i2'
                              // The index must be greater than 0 and there must not be two successive element indices in the path.

    // "Large" kinds: starting from here the low 3 bits must be 1.
    // This and all following kinds (we'll add in the future) cannot have a field index.
    case tailElements   =    0x07 // (0 << 3) | 0x7    A tail allocated element of a class: syntax `ct`
    case existential    =    0x0f // (1 << 3) | 0x7    A concrete value projected out of an existential: synatx 'x'
    case anyClassField  =    0x17 // (2 << 3) | 0x7    Any class field, including tail elements: syntax `c*`
    case anyIndexedElement = 0x1f // (3 << 3) | 0x7    An unknown offset into an array of elements.
                                  // There must not be two successive element indices in the path.
    case anyValueFields =    0x27 // (4 << 3) | 0x7    Any number of any value fields (struct, tuple, enum): syntax `v**`
    case anything       =    0x2f // (5 << 3) | 0x7    Any number of any fields: syntax `**`

    public var isValueField: Bool {
      switch self {
        case .anyValueFields, .structField, .tupleField, .enumCase, .indexedElement, .anyIndexedElement, .existential:
          return true
        case .root, .anything, .anyClassField, .classField, .tailElements:
          return false
      }
    }

    public var isClassField: Bool {
      switch self {
        case .anyClassField, .classField, .tailElements:
          return true
        case .root, .anything, .anyValueFields, .structField, .tupleField, .enumCase, .indexedElement, .anyIndexedElement, .existential:
          return false
      }
    }

    public var isIndexedElement: Bool {
      switch self {
      case .anyIndexedElement, .indexedElement:
        return true
      default:
        return false
      }
    }
  }

  public init() { self.bytes = 0 }

  /// Creates a new path with an initial element.
  public init(_ kind: FieldKind, index: Int = 0) {
    self = Self().push(kind, index: index)
  }

  private init(bytes: UInt64) { self.bytes = bytes }

  public var isEmpty: Bool { bytes == 0 }

  public var description: String {
    let (kind, idx, sp) = pop()
    let subPath = sp
    let s: String
    switch kind {
      case .root:           return ""
      case .structField:    s = "s\(idx)"
      case .tupleField:     s = "\(idx)"
      case .enumCase:       s = "e\(idx)"
      case .classField:     s = "c\(idx)"
      case .tailElements:   s = "ct"
      case .existential:    s = "x"
      case .indexedElement: s = "i\(idx)"
      case .anyIndexedElement: s = "i*"
      case .anything:       s = "**"
      case .anyValueFields: s = "v**"
      case .anyClassField:  s = "c*"
    }
    if subPath.isEmpty {
      return s
    }
    return "\(s).\(subPath)"
  }

  /// Returns the top (= the first) path component and the number of its encoding bits.
  private var top: (kind: FieldKind, index: Int, numBits: Int) {
    var idx = 0
    var b = bytes
    var numBits = 0

    // Parse any index overflow bytes.
    while (b & 1) == 1 {
      idx = (idx << 7) | Int((b >> 1) &  0x7f)
      b >>= 8
      numBits = numBits &+ 8
    }
    var kindVal = (b >> 1) & 0x7
    if kindVal == 0x7 {
      // A "large" kind - without any index
      kindVal = (b >> 1) & 0x7f
      assert(idx == 0)
      assert(numBits == 0)
    } else {
      // A "small" kind with an index
      idx = (idx << 4) | Int((b >> 4) &  0xf)
    }
    let k = FieldKind(rawValue: Int(kindVal))!
    if k == .anything {
      assert((b >> 8) == 0, "'anything' only allowed in last path component")
      numBits = 8
    } else {
      numBits = numBits &+ 8
    }
    return (k, idx, numBits)
  }

  /// Pops \p numBits from the path.
  private func pop(numBits: Int) -> SmallProjectionPath {
    return Self(bytes: bytes &>> numBits)
  }

  /// Pops and returns the first path component included the resulting path
  /// after popping.
  ///
  /// For example, popping from `s0.c3.e1` returns (`s`, 0, `c3.e1`)
  public func pop() -> (kind: FieldKind, index: Int, path: SmallProjectionPath) {
    let (k, idx, numBits) = top
    return (k, idx, pop(numBits: numBits))
  }

  /// Pushes a new first component to the path and returns the new path.
  ///
  /// For example, pushing `s0` to `c3.e1` returns `s0.c3.e1`.
  public func push(_ kind: FieldKind, index: Int = 0) -> SmallProjectionPath {
    assert(kind != .anything || bytes == 0, "'anything' only allowed in last path component")
    if (kind.isIndexedElement) {
      let (k, i, numBits) = top
      if kind == .indexedElement {
        if index == 0 {
          // Ignore zero indices
          return self
        }
        if k == .indexedElement {
          // "Merge" two constant successive indexed elements
          return pop(numBits: numBits).push(.indexedElement, index: index + i)
        }
      }
      // "Merge" two successive indexed elements which doesn't have a constant result
      if (k.isIndexedElement) {
        return pop(numBits: numBits).push(.anyIndexedElement)
      }
    }
    var idx = index
    var b = bytes
    if (b >> 56) != 0 {
      // Overflow
      return Self(.anything)
    }
    b = (b << 8) | UInt64(((idx & 0xf) << 4) | (kind.rawValue << 1))
    idx >>= 4
    while idx != 0 {
      if (b >> 56) != 0 { return Self(.anything) }
      b = (b << 8) | UInt64(((idx & 0x7f) << 1) | 1)
      idx >>= 7
    }
    return Self(bytes: b)
  }

  /// Pops the first path component if it is exactly of kind `kind` - not considering wildcards.
  ///
  /// Returns the index of the component and the new path or - if not matching - returns nil.
  public func pop(kind: FieldKind) -> (index: Int, path: SmallProjectionPath)? {
    let (k, idx, newPath) = pop()
    if k != kind { return nil }
    return (idx, newPath)
  }

  /// Pops the first path component if it matches `kind` and (optionally) `index`.
  ///
  /// For example:
  /// popping `s0` from `s0.c3.e1` returns `c3.e1`
  /// popping `c2` from `c*.e1` returns `e1`
  /// popping `s0` from `v**.c3.e1` return `v**.c3.e1` (because `v**` means _any_ number of value fields)
  /// popping `s0` from `c*.e1` returns nil
  ///
  /// Note that if `kind` is a wildcard, also the first path component must be a wildcard to popped.
  /// For example:
  /// popping `v**` from `s0.c1` returns nil
  /// popping `v**` from `v**.c1` returns `v**.c1` (because `v**` means _any_ number of value fields)
  /// popping `c*`  from `c0.e3` returns nil
  /// popping `c*`  from `c*.e3` returns `e3`
  public func popIfMatches(_ kind: FieldKind, index: Int? = nil) -> SmallProjectionPath? {
    let (k, idx, numBits) = top
    switch k {
      case .anything:
        return self
      case .anyValueFields:
        if kind.isValueField { return self }
        return pop(numBits: numBits).popIfMatches(kind, index: index)
      case .anyClassField:
        if kind.isClassField {
          return pop(numBits: numBits)
        }
        return nil
      case .anyIndexedElement:
        if kind.isIndexedElement {
          return self
        }
        return pop(numBits: numBits).popIfMatches(kind, index: index)
      case kind:
        if let i = index {
          if i != idx { return nil }
        }
        return pop(numBits: numBits)
      default:
        return nil
    }
  }

  /// Returns true if the path has at least one class projection.
  /// For example:
  /// returns false for `v**`
  /// returns true for `v**.c0.s1.v**`
  /// returns false for `**` (because '**' can have zero class projections)
  public var hasClassProjection: Bool {
    var p = self
    while true {
      let (k, _, numBits) = p.top
      if k == .root { return false }
      if k.isClassField { return true }
      p = p.pop(numBits: numBits)
    }
  }

  /// Returns true if the path may have a class projection.
  /// For example:
  /// returns false for `v**`
  /// returns true for `c0`
  /// returns true for `**` (because '**' can have any number of class projections)
  public var mayHaveClassProjection: Bool {
    return !matches(pattern: Self(.anyValueFields))
  }

  /// Returns true if the path may have a class projection.
  /// For example:
  /// returns false for `v**`
  /// returns false for `c0`
  /// returns true for `c0.c1`
  /// returns true for `c0.**` (because '**' can have any number of class projections)
  /// returns true for `**` (because '**' can have any number of class projections)
  public var mayHaveTwoClassProjections: Bool {
    return !matches(pattern: Self(.anyValueFields)) &&
           !matches(pattern: Self(.anyValueFields).push(.anyClassField).push(.anyValueFields))
  }

  /// Pops all value field components from the beginning of the path.
  /// For example:
  ///    `s0.e2.3.c4.s1` -> `c4.s1`
  ///    `v**.c4.s1`     -> `c4.s1`
  ///    `**`            -> `**` (because `**` can also be a class field)
  public func popAllValueFields() -> SmallProjectionPath {
    var p = self
    while true {
      let (k, _, numBits) = p.top
      if !k.isValueField { return p }
      p = p.pop(numBits: numBits)
    }
  }

  public func popIndexedElements() -> SmallProjectionPath {
    var p = self
    while true {
      let (k, _, numBits) = p.top
      if !k.isIndexedElement { return p }
      p = p.pop(numBits: numBits)
    }
  }

  /// Pops the last class projection and all following value fields from the tail of the path.
  /// For example:
  ///    `s0.e2.3.c4.s1` -> `s0.e2.3`
  ///    `v**.c1.c4.s1`  -> `v**.c1`
  ///    `c1.**`         -> `c1.**` (because it's unknown how many class projections are in `**`)
  public func popLastClassAndValuesFromTail() -> SmallProjectionPath {
    var p = self
    var totalBits = 0
    var neededBits = 0
    while true {
      let (k, _, numBits) = p.top
      if k == .root { break }
      if k.isClassField {
        neededBits = totalBits
        totalBits += numBits
      } else {
        totalBits += numBits
        if !k.isValueField {
          // k is `anything`
          neededBits = totalBits
        }
      }
      p = p.pop(numBits: numBits)
    }
    if neededBits == 64 { return self }
    return SmallProjectionPath(bytes: bytes & ((1 << neededBits) - 1))
  }

  /// Returns true if this path matches a pattern path.
  ///
  /// Formally speaking:
  /// If this path is a concrete path, returns true if it matches the pattern.
  /// If this path is a pattern path itself, returns true if all concrete paths which
  /// match this path also match the pattern path.
  /// For example:
  ///    `s0.c3.e1` matches `s0.c3.e1`
  ///    `s0.c3.e1` matches `v**.c*.e1`
  ///    `v**.c*.e1` does not match `s0.c3.e1`!
  ///  Note that matching is not reflexive.
  public func matches(pattern: SmallProjectionPath) -> Bool {
    let (patternKind, patternIdx, subPattern) = pattern.pop()
    switch patternKind {
      case .root:          return isEmpty
      case .anything:      return true
      case .anyValueFields:
        return popAllValueFields().matches(pattern: subPattern)
      case .anyClassField:
        let (kind, _, subPath) = pop()
        if !kind.isClassField { return false }
        return subPath.matches(pattern: subPattern)
      case .anyIndexedElement:
        return popIndexedElements().matches(pattern: subPattern)
      case .structField, .tupleField, .enumCase, .classField, .tailElements, .indexedElement, .existential:
        let (kind, index, subPath) = pop()
        if kind != patternKind || index != patternIdx { return false }
        return subPath.matches(pattern: subPattern)
    }
  }

  /// Returns the merged path of this path and `rhs`.
  ///
  /// Merging means that all paths which match this path and `rhs` will also match the result.
  /// If `rhs` is not equal to this path, the result is computed by replacing
  /// mismatching components by wildcards.
  /// For example:
  ///    `s0.c3.e4` merged with `s0.c1.e4` -> `s0.c*.e4`
  ///    `s0.s1.c3` merged with `e4.c3`    -> `v**.c3`
  ///    `s0.c1.c2` merged with `s0.c3`    -> `s0.**`
  public func merge(with rhs: SmallProjectionPath) -> SmallProjectionPath {
    if self == rhs { return self }

    let (lhsKind, lhsIdx, lhsBits) = top
    let (rhsKind, rhsIdx, rhsBits) = rhs.top
    if lhsKind == rhsKind && lhsIdx == rhsIdx {
      assert(lhsBits == rhsBits)
      let subPath = pop(numBits: lhsBits).merge(with: rhs.pop(numBits: rhsBits))
      if lhsKind == .anyValueFields && subPath.top.kind == .anyValueFields {
        return subPath
      }
      return subPath.push(lhsKind, index: lhsIdx)
    }
    if lhsKind.isIndexedElement || rhsKind.isIndexedElement {
      let subPath = popIndexedElements().merge(with: rhs.popIndexedElements())
      let subPathTopKind = subPath.top.kind
      assert(!subPathTopKind.isIndexedElement)
      if subPathTopKind == .anything || subPathTopKind == .anyValueFields {
        return subPath
      }
      return subPath.push(.anyIndexedElement)
    }
    if lhsKind.isValueField || rhsKind.isValueField {
      let subPath = popAllValueFields().merge(with: rhs.popAllValueFields())
      assert(!subPath.top.kind.isValueField)
      if subPath.top.kind == .anything {
        return subPath
      }
      return subPath.push(.anyValueFields)
    }
    if lhsKind.isClassField && rhsKind.isClassField {
      let subPath = pop(numBits: lhsBits).merge(with: rhs.pop(numBits: rhsBits))
      return subPath.push(.anyClassField)
    }
    return Self(.anything)
  }

  /// Returns true if this path may overlap with `rhs`.
  ///
  /// "Overlapping" means that both paths may project the same field.
  /// For example:
  ///    `s0.s1`  and `s0.s1` overlap (the paths are identical)
  ///    `s0.s1`  and `s0.s2` don't overlap
  ///    `s0.s1`  and `s0`    overlap (the second path is a sub-path of the first one)
  ///    `s0.v**` and `s0.s1` overlap
  public func mayOverlap(with rhs: SmallProjectionPath) -> Bool {
    if isEmpty || rhs.isEmpty {
      return true
    }

    let (lhsKind, lhsIdx, lhsBits) = top
    let (rhsKind, rhsIdx, rhsBits) = rhs.top

    if lhsKind == .anything || rhsKind == .anything {
      return true
    }
    if lhsKind == .anyIndexedElement || rhsKind == .anyIndexedElement {
      return popIndexedElements().mayOverlap(with: rhs.popIndexedElements())
    }
    if lhsKind == .anyValueFields || rhsKind == .anyValueFields {
      return popAllValueFields().mayOverlap(with: rhs.popAllValueFields())
    }
    if (lhsKind == rhsKind && lhsIdx == rhsIdx) ||
       (lhsKind == .anyClassField && rhsKind.isClassField) ||
       (lhsKind.isClassField && rhsKind == .anyClassField) {
      return pop(numBits: lhsBits).mayOverlap(with: rhs.pop(numBits: rhsBits))
    }
    return false
  }

  /// Subtracts this path from a larger path if this path is a prefix of the other path.
  ///
  /// For example:
  ///   subtracting `s0` from `s0.s1` yields `s1`
  ///   subtracting `s0` from `s1` yields nil, because `s0` is not a prefix of `s1`
  ///   subtracting `s0.s1` from `s0.s1` yields an empty path
  ///   subtracting `i*.s1` from `i*.s1` yields nil, because the actual index is unknown on both sides
  public func subtract(from rhs: SmallProjectionPath) -> SmallProjectionPath? {
    let (lhsKind, lhsIdx, lhsBits) = top
    switch lhsKind {
    case .root:
      return rhs
    case .classField, .tailElements, .structField, .tupleField, .enumCase, .existential, .indexedElement:
      let (rhsKind, rhsIdx, rhsBits) = rhs.top
      if lhsKind == rhsKind && lhsIdx == rhsIdx {
        return pop(numBits: lhsBits).subtract(from: rhs.pop(numBits: rhsBits))
      }
      return nil
    case .anything, .anyValueFields, .anyClassField, .anyIndexedElement:
      return nil
    }
  }

  /// Returns true if the path only contains projections which can be materialized as
  /// SIL struct or tuple projection instructions - for values or addresses.
  public var isMaterializable: Bool {
    let (kind, _, subPath) = pop()
    switch kind {
    case .root:
      return true
    case .structField, .tupleField:
      return subPath.isMaterializable
    default:
      return false
    }
  }
}

//===----------------------------------------------------------------------===//
//                               Parsing
//===----------------------------------------------------------------------===//

extension StringParser {

  mutating func parseProjectionPathFromSource(for function: Function, type: Type?) throws -> SmallProjectionPath {
    var entries: [(SmallProjectionPath.FieldKind, Int)] = []
    var currentTy = type
    repeat {
      if consume("**") {
        entries.append((.anything, 0))
        currentTy = nil
      } else if consume("class*") {
        if let ty = currentTy, !ty.isClass {
          try throwError("cannot use 'anyClassField' on a non-class type - add 'anyValueFields' first")
        }
        entries.append((.anyClassField, 0))
        currentTy = nil
      } else if consume("value**") {
        entries.append((.anyValueFields, 0))
        currentTy = nil
      } else if let tupleElemIdx = consumeInt() {
        guard let ty = currentTy, ty.isTuple else {
          try throwError("cannot use a tuple index after 'any' field selection")
        }
        let tupleElements = ty.tupleElements
        if tupleElemIdx >= tupleElements.count {
          try throwError("tuple element index too large")
        }
        entries.append((.tupleField, tupleElemIdx))
        currentTy = tupleElements[tupleElemIdx]
      } else if let name = consumeIdentifier() {
        guard let ty = currentTy else {
          try throwError("cannot use field name after 'any' field selection")
        }
        if !ty.isClass && !ty.isStruct {
          try throwError("unknown kind of nominal type")
        }
        guard let nominalFields = ty.getNominalFields(in: function) else {
          try throwError("resilient types are not supported")
        }
        guard let fieldIdx = nominalFields.getIndexOfField(withName: name) else {
          try throwError("field not found")
        }
        if ty.isClass {
          entries.append((.classField, fieldIdx))
        } else {
          assert(ty.isStruct)
          entries.append((.structField, fieldIdx))
        }
        currentTy = nominalFields[fieldIdx]
      } else {
        try throwError("expected selection path component")
      }
    } while consume(".")

    if let ty = currentTy, !ty.isClass {
      try throwError("the select field is not a class - add 'anyValueFields'")
    }

    return try createPath(from: entries)
  }

  mutating func parseProjectionPathFromSIL() throws -> SmallProjectionPath {
    var entries: [(SmallProjectionPath.FieldKind, Int)] = []
    while true {
      if consume("**") {
        entries.append((.anything, 0))
      } else if consume("c*") {
        entries.append((.anyClassField, 0))
      } else if consume("v**") {
        entries.append((.anyValueFields, 0))
      } else if consume("i*") {
        entries.append((.anyIndexedElement, 0))
      } else if consume("ct") {
        entries.append((.tailElements, 0))
      } else if consume("x") {
        entries.append((.existential, 0))
      } else if consume("c") {
        guard let idx = consumeInt(withWhiteSpace: false) else {
          try throwError("expected class field index")
        }
        entries.append((.classField, idx))
      } else if consume("e") {
        guard let idx = consumeInt(withWhiteSpace: false) else {
          try throwError("expected enum case index")
        }
        entries.append((.enumCase, idx))
      } else if consume("s") {
        guard let idx = consumeInt(withWhiteSpace: false) else {
          try throwError("expected struct field index")
        }
        entries.append((.structField, idx))
      } else if consume("i") {
        guard let idx = consumeInt(withWhiteSpace: false) else {
          try throwError("expected index")
        }
        entries.append((.indexedElement, idx))
      } else if let tupleElemIdx = consumeInt() {
        entries.append((.tupleField, tupleElemIdx))
      } else if !consume(".") {
        return try createPath(from: entries)
      }
    }
  }

  private func createPath(from entries: [(SmallProjectionPath.FieldKind, Int)]) throws -> SmallProjectionPath {
    var path = SmallProjectionPath()
    var first = true
    for (kind, idx) in entries.reversed() {
      if !first && kind == .anything {
        try throwError("'**' only allowed in last path component")
      }
      path = path.push(kind, index: idx)

      // Check for overflow
      if !first && path == SmallProjectionPath(.anything) {
        try throwError("path is too long")
      }
      first = false
    }
    return path
  }
}

//===----------------------------------------------------------------------===//
//                               Unit Tests
//===----------------------------------------------------------------------===//

extension SmallProjectionPath {
  public static func runUnitTests() {

    basicPushPop()
    parsing()
    merging()
    subtracting()
    matching()
    overlapping()
    predicates()
    path2path()

    func basicPushPop() {
      let p1 = SmallProjectionPath(.structField, index: 3)
                        .push(.classField, index: 12345678)
      let (k2, i2, p2) = p1.pop()
      assert(k2 == .classField && i2 == 12345678)
      let (k3, i3, p3) = p2.pop()
      assert(k3 == .structField && i3 == 3)
      assert(p3.isEmpty)
      let (k4, i4, _) = p2.push(.enumCase, index: 876).pop()
      assert(k4 == .enumCase && i4 == 876)
      let p5 = SmallProjectionPath(.anything)
      assert(p5.pop().path.isEmpty)
      let p6 = SmallProjectionPath(.indexedElement, index: 1).push(.indexedElement, index: 2)
      let (k6, i6, p7) = p6.pop()
      assert(k6 == .indexedElement && i6 == 3 && p7.isEmpty)
      let p8 = SmallProjectionPath(.indexedElement, index: 0)
      assert(p8.isEmpty)
      let p9 = SmallProjectionPath(.indexedElement, index: 1).push(.anyIndexedElement)
      let (k9, i9, p10) = p9.pop()
      assert(k9 == .anyIndexedElement && i9 == 0 && p10.isEmpty)
      let p11 = SmallProjectionPath(.anyIndexedElement).push(.indexedElement, index: 1)
      let (k11, i11, p12) = p11.pop()
      assert(k11 == .anyIndexedElement && i11 == 0 && p12.isEmpty)
    }

    func parsing() {
      testParse("v**.c*", expect: SmallProjectionPath(.anyClassField)
                                         .push(.anyValueFields))
      testParse("s3.c*.v**.s1", expect: SmallProjectionPath(.structField, index: 1)
                                         .push(.anyValueFields)
                                         .push(.anyClassField)
                                         .push(.structField, index: 3))
      testParse("2.c*.e6.ct.**", expect: SmallProjectionPath(.anything)
                                         .push(.tailElements)
                                         .push(.enumCase, index: 6)
                                         .push(.anyClassField)
                                         .push(.tupleField, index: 2))
      testParse("i3.x.i*", expect: SmallProjectionPath(.anyIndexedElement)
                                         .push(.existential)
                                         .push(.indexedElement, index: 3))

      do {
        var parser = StringParser("c*.s123.s3.s123.s3.s123.s3.s123.s3.s123.s3.s123.s3.s123.s3.s123.s3.s123.s3.s123.s3.s123.s3.s123.s3.s123.s3.**")
        _ = try parser.parseProjectionPathFromSIL()
        fatalError("too long path not detected")
      } catch {
      }
      do {
        var parser = StringParser("**.s0")
        _ = try parser.parseProjectionPathFromSIL()
        fatalError("wrong '**' not detected")
      } catch {
      }
    }

    func testParse(_ pathStr: String, expect: SmallProjectionPath) {
      var parser = StringParser(pathStr)
      let path = try! parser.parseProjectionPathFromSIL()
      assert(path == expect)
      let str = path.description
      assert(str == pathStr)
    }

    func merging() {
      testMerge("c1.c0",    "c0",     expect: "c*.**")
      testMerge("c2.c1",    "c2",     expect: "c2.**")
      testMerge("s3.c0",    "v**.c0", expect: "v**.c0")
      testMerge("c0",       "s2.c1",  expect: "v**.c*")
      testMerge("s1.s1.c2", "s1.c2",  expect: "s1.v**.c2")
      testMerge("s1.s0",    "s2.s0",  expect: "v**")
      testMerge("ct",       "c2",     expect: "c*")
      testMerge("i1",       "i2",     expect: "i*")
      testMerge("i*",       "i2",     expect: "i*")
      testMerge("s0.i*.e3", "s0.e3",  expect: "s0.i*.e3")
      testMerge("i*",       "v**",    expect: "v**")

      testMerge("ct.s0.e0.v**.c0", "ct.s0.e0.v**.c0", expect: "ct.s0.e0.v**.c0")
      testMerge("ct.s0.s0.c0",     "ct.s0.e0.s0.c0",  expect: "ct.s0.v**.c0")
    }

    func testMerge(_ lhsStr: String, _ rhsStr: String,
                   expect expectStr: String) {
      var lhsParser = StringParser(lhsStr)
      let lhs = try! lhsParser.parseProjectionPathFromSIL()
      var rhsParser = StringParser(rhsStr)
      let rhs = try! rhsParser.parseProjectionPathFromSIL()
      var expectParser = StringParser(expectStr)
      let expect = try! expectParser.parseProjectionPathFromSIL()

      let result = lhs.merge(with: rhs)
      assert(result == expect)
      let result2 = rhs.merge(with: lhs)
      assert(result2 == expect)
    }

    func subtracting() {
      testSubtract("s0",           "s0.s1",        expect: "s1")
      testSubtract("s0",           "s1",           expect: nil)
      testSubtract("s0.s1",        "s0.s1",        expect: "")
      testSubtract("i*.s1",        "i*.s1",        expect: nil)
      testSubtract("ct.s1.0.i3.x", "ct.s1.0.i3.x", expect: "")
      testSubtract("c0.s1.0.i3",   "c0.s1.0.i3.x", expect: "x")
      testSubtract("s1.0.i3.x",    "s1.0.i3",      expect: nil)
      testSubtract("v**.s1",       "v**.s1",       expect: nil)
      testSubtract("i*",           "i*",           expect: nil)
    }

    func testSubtract(_ lhsStr: String, _ rhsStr: String, expect expectStr: String?) {
      var lhsParser = StringParser(lhsStr)
      let lhs = try! lhsParser.parseProjectionPathFromSIL()
      var rhsParser = StringParser(rhsStr)
      let rhs = try! rhsParser.parseProjectionPathFromSIL()

      let result = lhs.subtract(from: rhs)

      if let expectStr = expectStr {
        var expectParser = StringParser(expectStr)
        let expect = try! expectParser.parseProjectionPathFromSIL()
        assert(result! == expect)
      } else {
        assert(result == nil)
      }
    }

    func matching() {
      testMatch("ct", "c*",  expect: true)
      testMatch("c1", "c*",  expect: true)
      testMatch("s2", "v**", expect: true)
      testMatch("1",  "v**", expect: true)
      testMatch("e1", "v**", expect: true)
      testMatch("c*", "c1",  expect: false)
      testMatch("c*", "ct",  expect: false)
      testMatch("v**", "s0", expect: false)
      testMatch("i1", "i1",  expect: true)
      testMatch("i1", "i*",  expect: true)
      testMatch("i*", "i1",  expect: false)

      testMatch("s0.s1", "s0.s1",     expect: true)
      testMatch("s0.s2", "s0.s1",     expect: false)
      testMatch("s0", "s0.v**",       expect: true)
      testMatch("s0.s1", "s0.v**",    expect: true)
      testMatch("s0.1.e2", "s0.v**",  expect: true)
      testMatch("s0.v**.x.e2", "v**", expect: true)
      testMatch("s0.v**", "s0.s1",    expect: false)
      testMatch("s0.s1.c*", "s0.v**", expect: false)
      testMatch("s0.v**", "s0.**",    expect: true)
      testMatch("s1.v**", "s0.**",    expect: false)
      testMatch("s0.**", "s0.v**",    expect: false)
      testMatch("s0.s1", "s0.i*.s1",  expect: true)
    }

    func testMatch(_ lhsStr: String, _ rhsStr: String, expect: Bool) {
      var lhsParser = StringParser(lhsStr)
      let lhs = try! lhsParser.parseProjectionPathFromSIL()
      var rhsParser = StringParser(rhsStr)
      let rhs = try! rhsParser.parseProjectionPathFromSIL()
      let result = lhs.matches(pattern: rhs)
      assert(result == expect)
    }

    func overlapping() {
      testOverlap("s0.s1.s2", "s0.s1.s2",     expect: true)
      testOverlap("s0.s1.s2", "s0.s2.s2",     expect: false)
      testOverlap("s0.s1.s2", "s0.e1.s2",     expect: false)
      testOverlap("s0.s1.s2", "s0.s1",        expect: true)
      testOverlap("s0.s1.s2", "s1.s2",        expect: false)

      testOverlap("s0.c*.s2", "s0.ct.s2",     expect: true)
      testOverlap("s0.c*.s2", "s0.c1.s2",     expect: true)
      testOverlap("s0.c*.s2", "s0.c1.c2.s2",  expect: false)
      testOverlap("s0.c*.s2", "s0.s2",        expect: false)

      testOverlap("s0.v**.s2", "s0.s3.x",     expect: true)
      testOverlap("s0.v**.s2.c2", "s0.s3.c1", expect: false)
      testOverlap("s0.v**.s2", "s1.s3",       expect: false)
      testOverlap("s0.v**.s2", "s0.v**.s3",   expect: true)

      testOverlap("s0.**", "s0.s3.c1",        expect: true)
      testOverlap("**", "s0.s3.c1",           expect: true)

      testOverlap("i1", "i*",                 expect: true)
      testOverlap("i1", "v**",                expect: true)
      testOverlap("s0.i*.s1", "s0.s1",        expect: true)
    }

    func testOverlap(_ lhsStr: String, _ rhsStr: String, expect: Bool) {
      var lhsParser = StringParser(lhsStr)
      let lhs = try! lhsParser.parseProjectionPathFromSIL()
      var rhsParser = StringParser(rhsStr)
      let rhs = try! rhsParser.parseProjectionPathFromSIL()
      let result = lhs.mayOverlap(with: rhs)
      assert(result == expect)
      let reversedResult = rhs.mayOverlap(with: lhs)
      assert(reversedResult == expect)
    }

    func predicates() {
      testPredicate("v**",           \.hasClassProjection, expect: false)
      testPredicate("v**.c0.s1.v**", \.hasClassProjection, expect: true)
      testPredicate("c0.**",         \.hasClassProjection, expect: true)
      testPredicate("c0.c1",         \.hasClassProjection, expect: true)
      testPredicate("ct",            \.hasClassProjection, expect: true)
      testPredicate("s0",            \.hasClassProjection, expect: false)

      testPredicate("v**", \.mayHaveClassProjection, expect: false)
      testPredicate("c0",  \.mayHaveClassProjection, expect: true)
      testPredicate("1",   \.mayHaveClassProjection, expect: false)
      testPredicate("**",  \.mayHaveClassProjection, expect: true)

      testPredicate("v**", \.mayHaveTwoClassProjections, expect: false)
      testPredicate("c0",  \.mayHaveTwoClassProjections, expect: false)
      testPredicate("**",  \.mayHaveTwoClassProjections, expect: true)
      testPredicate("v**.c*.s2.1.c0",   \.mayHaveTwoClassProjections, expect: true)
      testPredicate("c*.s2.1.c0.v**",   \.mayHaveTwoClassProjections, expect: true)
      testPredicate("v**.c*.**",        \.mayHaveTwoClassProjections, expect: true)
    }

    func testPredicate(_ pathStr: String, _ property: (SmallProjectionPath) -> Bool, expect: Bool) {
      var parser = StringParser(pathStr)
      let path = try! parser.parseProjectionPathFromSIL()
      let result = property(path)
      assert(result == expect)
    }

    func path2path() {
      testPath2Path("s0.e2.3.c4.s1", { $0.popAllValueFields() }, expect: "c4.s1")
      testPath2Path("v**.c4.s1",     { $0.popAllValueFields() }, expect: "c4.s1")
      testPath2Path("**",            { $0.popAllValueFields() }, expect: "**")

      testPath2Path("s0.e2.3.c4.s1.e2.v**.**", { $0.popLastClassAndValuesFromTail() }, expect: "s0.e2.3.c4.s1.e2.v**.**")
      testPath2Path("s0.c2.3.c4.s1",           { $0.popLastClassAndValuesFromTail() }, expect: "s0.c2.3")
      testPath2Path("v**.c*.s1",               { $0.popLastClassAndValuesFromTail() }, expect: "v**")
      testPath2Path("s1.ct.v**",               { $0.popLastClassAndValuesFromTail() }, expect: "s1")
      testPath2Path("c0.c1.c2",                { $0.popLastClassAndValuesFromTail() }, expect: "c0.c1")
      testPath2Path("**",                      { $0.popLastClassAndValuesFromTail() }, expect: "**")

      testPath2Path("v**.c3", { $0.popIfMatches(.anyValueFields) }, expect: "v**.c3")
      testPath2Path("**",     { $0.popIfMatches(.anyValueFields) }, expect: "**")
      testPath2Path("s0.c3",  { $0.popIfMatches(.anyValueFields) }, expect: nil)

      testPath2Path("c0.s3",  { $0.popIfMatches(.anyClassField) }, expect: nil)
      testPath2Path("**",     { $0.popIfMatches(.anyClassField) }, expect: "**")
      testPath2Path("c*.e3",  { $0.popIfMatches(.anyClassField) }, expect: "e3")

      testPath2Path("i*.e3.s0", { $0.popIfMatches(.enumCase, index: 3) }, expect: "s0")
      testPath2Path("i1.e3.s0", { $0.popIfMatches(.enumCase, index: 3) }, expect: nil)
      testPath2Path("i*.e3.s0", { $0.popIfMatches(.indexedElement, index: 0) }, expect: "i*.e3.s0")
    }

    func testPath2Path(_ pathStr: String, _ transform: (SmallProjectionPath) -> SmallProjectionPath?, expect: String?) {
      var parser = StringParser(pathStr)
      let path = try! parser.parseProjectionPathFromSIL()
      let result = transform(path)
      if let expect = expect {
        var expectParser = StringParser(expect)
        let expectPath = try! expectParser.parseProjectionPathFromSIL()
        assert(result == expectPath)
      } else {
        assert(result == nil)
      }
    }
  }
}
