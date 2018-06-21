
struct Pattern<P: BidirectionalCollection>
  where P.Element: Hashable
{
  var pattern: P
  
  func firstRange<C: BidirectionalCollection>
    (in collection: C, startingAt: C.Index? = nil) -> Range<C.Index>?
    where C.Element == P.Element
  {
    if pattern.isEmpty || collection.isEmpty {
      return nil
    }
    
    let start = startingAt ?? collection.startIndex
    let boyerMooreSearch = BoyerMooreSearch(haystack: collection, needle: pattern)
    
    return boyerMooreSearch.findFirstRange(startingAt: start)
  }
}

struct BoyerMooreSearch<C: BidirectionalCollection, P: BidirectionalCollection>
  where C.Element: Hashable, C.Element == P.Element
{
  typealias Haystack = C
  typealias Needle = P
  
  var haystack: C
  var needle: P
  let context: BoyerMooreContext<Needle>
  
  init(haystack: C, needle: P) {
    self.haystack = haystack
    self.needle = needle
    self.context = BoyerMooreContext(pattern: needle)
  }
  
  func findFirstRange(startingAt: C.Index) -> Range<C.Index>? {
    if context.patternLength > haystack.count {
      return nil
    }
    
    let needleEnd = needle.index(before: needle.endIndex)
    let needleStart = needle.startIndex
    let cachedEndIndex = haystack.endIndex
    
    guard var end = haystack.index(startingAt, offsetBy: context.patternLength - 1, limitedBy: cachedEndIndex) else {
      return nil
    }
    
    var currentIndex = end
    var currentNeedleIndex = needleEnd
    
    while currentIndex < cachedEndIndex {
      while haystack[currentIndex] == needle[currentNeedleIndex] {
        if currentNeedleIndex == needleStart {
          haystack.formIndex(after: &end)
          return currentIndex..<end
        }
        haystack.formIndex(before: &currentIndex)
        needle.formIndex(before: &currentNeedleIndex)
      }
      let position = needle.distance(from: currentNeedleIndex, to: needle.endIndex) - 1
      let distanceToMove = Swift.max(
        context.badMatchTable[haystack[end], default: context.patternLength],
        context.goodSuffixTable[position]
      )
      
      end = haystack.index(currentIndex, offsetBy: distanceToMove, limitedBy: cachedEndIndex) ?? cachedEndIndex
      currentIndex = end
      currentNeedleIndex = needleEnd
    }
    
    return nil
  }
}

internal struct BoyerMooreContext<C: BidirectionalCollection> where C.Element: Hashable {
  var badMatchTable: [C.Element: Int]
  var goodSuffixTable: [Int]
  var patternLength: Int
  
  init(pattern: C) {
    let needleLength = pattern.count
    func constructBadMatchTable() -> [C.Element: Int] {
      var dict: [C.Element: Int] = [:]
      
      for (i, e) in pattern.enumerated() {
        let value = needleLength - i - 1
        
        if i == needleLength - 1 {
          if dict[e] == nil {
            dict[e] = needleLength
          }
          break
        }
        dict[e] = value
      }
      return dict
    }
    
    func suffixLength(_ p: Int) -> Int {
      let index = pattern.index(pattern.endIndex, offsetBy: -p - 1)
      let zipped = zip(pattern[index...], pattern).reversed()
      
      var count = 0
      
      for (left, right) in zipped {
        if left != right {
          break
        }
        count += 1
      }
      
      return count
    }
    
    func constructGoodSuffixTable() -> [Int] {
      var array = [Int](repeating: 0, count: needleLength)
      
      var lastPosition = needleLength
      for i in (1..<needleLength).reversed() {
        if pattern.prefix(i).elementsEqual(pattern.suffix(i)) {
          lastPosition = i
        }
        array[needleLength - i] = lastPosition - i + needleLength
      }
      for i in 0..<(needleLength - 1) {
        let length = suffixLength(i)
        array[length] = needleLength - 1 - i + length
      }
      
      return array
    }
    
    badMatchTable = constructBadMatchTable()
    goodSuffixTable = constructGoodSuffixTable()
    patternLength = needleLength
  }
}

public struct LazyRangeCollection<Base: Collection, Other: BidirectionalCollection> where Base.Element == Other.Element, Base.Element: Hashable {
  let base: Base
  let pattern: Pattern<Other>
  let overlapping: Bool
  
  var offset: Base.Index
  
  public //should be internal
  init(_ base: Base, _ pattern: Other, overlapping: Bool) {
    self.base = base
    self.pattern = Pattern(pattern: pattern)
    self.overlapping = overlapping
    self.offset = base.startIndex
  }
}

extension LazyRangeCollection: Collection {
  public struct _Index: Comparable {
    var range: Range<Base.Index>
    
    public static func < (lhs: _Index, rhs: _Index) -> Bool {
      return lhs.range.lowerBound < rhs.range.lowerBound
    }
  }
  
  public typealias Index = _Index
  public typealias Element = Range<Base.Index>

  public var startIndex: Index {
    guard let range = base.firstRange(of: pattern.pattern) else {
      return endIndex
    }
    let index = _Index(range: range)
    return index
  }
  
  public var endIndex: Index {
    return _Index(range: base.endIndex..<base.endIndex)
  }

  public subscript(index: Index) -> Element {
    precondition(index < endIndex)
    return index.range
  }

  public func index(after i: _Index) -> _Index {
    let start: Base.Index
    if overlapping {
      var idx = i.range.lowerBound
      base.formIndex(after: &idx)
      start = idx
    } else {
      start = i.range.upperBound
    }
    
//    let pat = Pattern(pattern: pattern)
//    let ran = base._firstRange(of: <#T##BidirectionalCollection#>, startingAt: <#T##Comparable#>)
//    guard let range = base._firstRange(of: pattern.pattern, startingAt: start) else {
//      return endIndex
//    }
//    base._firstRange2(of: pattern, startingAt: start)
    guard let range = base._firstRange(of: pattern.pattern, startingAt: start) else {
      return endIndex
    }
    return _Index(range: range)
  }
}

extension LazyRangeCollection: BidirectionalCollection where Base: BidirectionalCollection {
  public func index(before i: _Index) -> _Index {
    _precondition(i > startIndex, "Can't move before startIndex")
    
    let start: Base.Index
    if overlapping {
      var idx = i.range.upperBound
      base.formIndex(before: &idx)
      start = idx
    } else {
      start = i.range.lowerBound
    }
    
    guard let range = base[..<start].lastRange(of: pattern.pattern) else {
      fatalError("Invariant broken, index was after startIndex but we didn't find startIndex before it")
    }
    
    return _Index(range: range)
  }
}

public enum SeparatorBehavior {
  case excluded
  case included
  case suffixPrevious
  case prefixSubsequent
}

public struct LazySplitCollection<Base: Collection, Other: BidirectionalCollection>: Sequence where Base.Element == Other.Element, Base.Element: Hashable {
  let base: Base
  let separator: Other
  let omittingEmptySubsequences: Bool
  let separatorBehavior: SeparatorBehavior
  let cachedEndIndex: Base.Index
  let cachedSeparatorCount: Int
  
  public //should be internal
  init(_ base: Base, separator: Other, omittingEmptySubsequences: Bool, behavior: SeparatorBehavior) {
    self.base = base
    self.separator = separator
    self.omittingEmptySubsequences = omittingEmptySubsequences
    self.separatorBehavior = behavior
    self.cachedEndIndex = base.endIndex
    
    //We only use this when including the separators as elements
    self.cachedSeparatorCount = behavior == .included ? separator.count : 0
  }
}

extension LazySplitCollection: Collection {
  public struct _Index: Comparable {
    internal enum ElementType {
      case separator
      case content
    }
    
    var range: Range<Base.Index>
    var type: ElementType
    
    init(range: Range<Base.Index>, type: ElementType = .content) {
      self.range = range
      self.type = type
    }
    
    public static func < (lhs: _Index, rhs: _Index) -> Bool {
      return lhs.range.lowerBound < rhs.range.lowerBound
          && lhs.range.upperBound <= rhs.range.upperBound
    }
  }
  
  public typealias Index = _Index
  public typealias Element = Base.SubSequence
  
  public var startIndex: Index {
    return index(afterBase: base.startIndex, type: nil)
  }
  
  public var endIndex: Index {
    return _Index(range: base.endIndex..<base.endIndex)
  }
  
  public subscript(index: Index) -> Element {
    precondition(index < endIndex)
    return base[index.range]
  }
  
  public func index(after i: _Index) -> _Index {
    return index(afterBase: i.range.upperBound, type: i.type)
  }
  
  private func index(afterBase i: Base.Index, type: _Index.ElementType?) -> _Index {
    if base.isEmpty || i == cachedEndIndex {
      return endIndex
    }
    
    var subSequenceStart = i
    var subSequenceEnd = subSequenceStart
    
    func nextRange(end: Base.Index) -> Range<Base.Index>? {
      if subSequenceStart == end && omittingEmptySubsequences {
        return nil
      }
      return subSequenceStart..<end
    }

    if let type = type, type == .content && separatorBehavior == .included {
      let start = i
      let end = base.index(start, offsetBy: cachedSeparatorCount)
      return _Index(range: start..<end, type: .separator)
    }
    
    while subSequenceEnd != cachedEndIndex {
      if let range = base._firstRange(of: separator, startingAt: subSequenceEnd) {
        let next: Range<Base.Index>?
        switch separatorBehavior {
        case .included:
          next = nextRange(end: range.lowerBound)
          (subSequenceStart, subSequenceEnd) = (range.upperBound, range.upperBound)
        case .excluded:
          next = nextRange(end: range.lowerBound)
          (subSequenceStart, subSequenceEnd) = (range.upperBound, range.upperBound)
        case .suffixPrevious:
          next = nextRange(end: range.upperBound)
          (subSequenceStart, subSequenceEnd) = (range.upperBound, range.upperBound)
        case .prefixSubsequent:
          next = nextRange(end: range.lowerBound)
          (subSequenceStart, subSequenceEnd) = (range.lowerBound, range.upperBound)
        }
      
        if let next = next {
          return _Index(range: next)
        }
        
        if separatorBehavior == .included {
          return _Index(range: range, type: .separator)
        }
      } else {
        break
      }
    }
    
    if subSequenceStart != cachedEndIndex || !omittingEmptySubsequences {
      defer { subSequenceStart = cachedEndIndex }
      return _Index(range: subSequenceStart..<cachedEndIndex)
    }
    
    return endIndex
  }
}

extension LazySplitCollection: BidirectionalCollection where Base: BidirectionalCollection {
  public func index(before i: _Index) -> _Index {
    if base.isEmpty || i == startIndex{
      fatalError("Cannot ask for the index before startIndex")
    }
    
    var subSequenceStart = i.range.lowerBound
    var subSequenceEnd = subSequenceStart
    
    func nextRange(start: Base.Index) -> Range<Base.Index>? {
      if start == subSequenceEnd && omittingEmptySubsequences {
        return nil
      }
      return start..<subSequenceEnd
    }
    
    if i.type == .content && separatorBehavior == .included && i != endIndex {
      let end = i.range.lowerBound
      
      if let start = base.index(end, offsetBy: -cachedSeparatorCount, limitedBy: base.startIndex) {
        let range = base.index(end, offsetBy: -cachedSeparatorCount)..<end
        return _Index(range: range, type: .separator)
      } else {
        return startIndex
      }
    }
    
    let cachedStartIndex = base.startIndex
    while subSequenceStart != cachedStartIndex {
      if let range = base[..<subSequenceStart].lastRange(of: separator) {
        let next: Range<Base.Index>?
        switch separatorBehavior {
        case .included:
          next = nextRange(start: range.upperBound)
          (subSequenceStart, subSequenceEnd) = (range.lowerBound, range.lowerBound)
        case .excluded:
          next = nextRange(start: range.upperBound)
          (subSequenceStart, subSequenceEnd) = (range.lowerBound, range.lowerBound)
        case .suffixPrevious:
          next = nextRange(start: range.upperBound)
          (subSequenceStart, subSequenceEnd) = (range.lowerBound, range.upperBound)
        case .prefixSubsequent:
          next = nextRange(start: range.lowerBound)
          (subSequenceStart, subSequenceEnd) = (range.lowerBound, range.lowerBound)
        }
        
        if let next = next {
          return _Index(range: next)
        }
        
        if separatorBehavior == .included {
          return _Index(range: range, type: .separator)
        }
      } else {
        break
      }
    }
    
    if subSequenceStart != cachedStartIndex || !omittingEmptySubsequences {
      defer { subSequenceStart = cachedEndIndex }
      let range = cachedStartIndex..<subSequenceEnd
      return _Index(range: range)
    }
    
    return startIndex
  }
}
