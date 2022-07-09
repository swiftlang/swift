extension String {
  /// Print out a full list of indices in every view of this string.
  /// This is useful while debugging string indexing issues.
  public func dumpIndices() {
    print("-------------------------------------------------------------------")
    print("String: \(String(reflecting: self))")
    print("Characters:")
    self.indices.forEach { i in
      let char = self[i]
      print("  \(i) -> \(String(reflecting: char))")
    }
    print("Scalars:")
    self.unicodeScalars.indices.forEach { i in
      let scalar = self.unicodeScalars[i]
      let value = String(scalar.value, radix: 16, uppercase: true)
      let padding = String(repeating: "0", count: max(0, 4 - value.count))
      let name = scalar.properties.name ?? "\(scalar.debugDescription)"
      print("  \(i) -> U+\(padding)\(value) \(name)")
    }
    print("UTF-8:")
    self.utf8.indices.forEach { i in
      let code = self.utf8[i]
      let value = String(code, radix: 16, uppercase: true)
      let padding = value.count < 2 ? "0" : ""
      print("  \(i) -> \(padding)\(value)")
    }
    print("UTF-16:")
    self.utf16.indices.forEach { i in
      let code = self.utf16[i]
      let value = String(code, radix: 16, uppercase: true)
      let padding = String(repeating: "0", count: 4 - value.count)
      print("  \(i) -> \(padding)\(value)")
    }
  }

  // Returns a list of every valid index in every string view, optionally
  // including end indices. We keep equal indices originating from different
  // views because they may have different grapheme size caches or flags etc.
  public func allIndices(includingEnd: Bool = true) -> [String.Index] {
    var r = Array(self.indices)
    if includingEnd { r.append(self.endIndex) }
    r += Array(self.unicodeScalars.indices)
    if includingEnd { r.append(self.unicodeScalars.endIndex) }
    r += Array(self.utf8.indices)
    if includingEnd { r.append(self.utf8.endIndex) }
    r += Array(self.utf16.indices)
    if includingEnd { r.append(self.utf16.endIndex) }
    return r
  }
}

extension Substring {
  // Returns a list of every valid index in every substring view, optionally
  // including end indices. We keep equal indices originating from different
  // views because they may have different grapheme size caches or flags etc.
  public func allIndices(includingEnd: Bool = true) -> [String.Index] {
    var r = Array(self.indices)
    if includingEnd { r.append(self.endIndex) }
    r += Array(self.unicodeScalars.indices)
    if includingEnd { r.append(self.unicodeScalars.endIndex) }
    r += Array(self.utf8.indices)
    if includingEnd { r.append(self.utf8.endIndex) }
    r += Array(self.utf16.indices)
    if includingEnd { r.append(self.utf16.endIndex) }
    return r
  }
}

extension Collection {
  // Assuming both `self` and `other` use the same index space, call `body` for
  // each index `i` in `other`, along with the slice in `self` that begins at
  // `i` and ends at the index following it in `other`.
  //
  // `other` must start with an item that is less than or equal to the first
  // item in `self`.
  func forEachIndexGroup<G: Collection>(
    by other: G,
    body: (G.Index, Self.SubSequence, Int) throws -> Void
  ) rethrows
  where G.Index == Self.Index
  {
    if other.isEmpty {
      assert(self.isEmpty)
      return
    }
    var i = other.startIndex
    var j = self.startIndex
    var offset = 0
    while i != other.endIndex {
      let current = i
      other.formIndex(after: &i)
      let start = j
      while j < i, j < self.endIndex {
        self.formIndex(after: &j)
      }
      let end = j
      try body(current, self[start ..< end], offset)
      offset += 1
    }
  }
}

extension String {
  /// Returns a dictionary mapping each valid index to the index that addresses
  /// the nearest scalar boundary, rounding down.
  public func scalarMap() -> [Index: (index: Index, offset: Int)] {
    var map: [Index: (index: Index, offset: Int)] = [:]

    utf8.forEachIndexGroup(by: unicodeScalars) { scalar, slice, offset in
      for i in slice.indices { map[i] = (scalar, offset) }
    }
    utf16.forEachIndexGroup(by: unicodeScalars) { scalar, slice, offset in
      for i in slice.indices { map[i] = (scalar, offset) }
    }
    self.forEachIndexGroup(by: unicodeScalars) { scalar, slice, offset in
      for i in slice.indices { map[i] = (scalar, offset) }
    }
    map[endIndex] = (endIndex, unicodeScalars.count)
    return map
  }

  /// Returns a dictionary mapping each valid index to the index that addresses
  /// the nearest character boundary, rounding down.
  public func characterMap() -> [Index: (index: Index, offset: Int)] {
    var map: [Index: (index: Index, offset: Int)] = [:]
    utf8.forEachIndexGroup(by: self) { char, slice, offset in
      for i in slice.indices { map[i] = (char, offset) }
    }
    utf16.forEachIndexGroup(by: self) { char, slice, offset in
      for i in slice.indices { map[i] = (char, offset) }
    }
    unicodeScalars.forEachIndexGroup(by: self) { char, slice, offset in
      for i in slice.indices { map[i] = (char, offset) }
    }
    map[endIndex] = (endIndex, count)
    return map
  }
}

