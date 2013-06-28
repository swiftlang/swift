struct RangeEnumerator<T: ForwardIndex> : Enumerator, Enumerable {
  typealias Element = T

  constructor(begin: T, end: T) {
    this.begin_.value = begin
    this.end_.value = end
  }

  func isEmpty() -> Bool {
    return begin_.value == end_.value
  }

  func next() -> Element {
    var ret = begin_.value
    begin_.value = begin_.value.succ()
    return ret
  }

  // Every Enumerator is also a single-pass Enumerable
  typealias EnumeratorType = RangeEnumerator<T>
  func getEnumeratorType() -> EnumeratorType {
    return this
  }

  var begin_, end_: GenericIVar<T>
}

struct Range<T: ForwardIndex> : Enumerable {  
  constructor(begin: T, end: T) {
    this.begin_.value = begin
    this.end_.value = end
  }

  func isEmpty() -> Bool {
    return begin_.value == end_.value
  }

  func begin() -> T {
    return begin_.value
  }
  func end() -> T {
    return end_.value
  }

  typealias EnumeratorType = RangeEnumerator<T>
  func getEnumeratorType() -> EnumeratorType {
    return EnumeratorType(begin_.value, end_.value)
  }

  var begin_, end_: GenericIVar<T>
}

/// \brief Any model of ForwardIndex can be turned into a Range with min..max
func .. <Pos: ForwardIndex> (min : Pos, max : Pos) -> Range<Pos> {
  return Range(min, max)
}

struct ReverseRangeEnumerator<T: BidirectionalIndex> : Enumerator, Enumerable {
  typealias Element = T

  constructor(begin: T, end: T) {
    this.begin_.value = begin
    this.end_.value = end
  }

  func isEmpty() -> Bool {
    return begin_.value == end_.value
  }

  func next() -> Element {
    end_.value = end_.value.pred()
    return end_.value
  }

  // Every Enumerator is also a single-pass Enumerable
  typealias EnumeratorType = ReverseRangeEnumerator<T>
  func getEnumeratorType() -> EnumeratorType {
    return this
  }

  var begin_, end_: GenericIVar<T>
}

struct ReverseRange<T: BidirectionalIndex> : Enumerable {  
  constructor(begin: T, end: T) {
    this.begin_.value = begin
    this.end_.value = end
  }

  constructor(fwd: Range<T>) {
    this.begin_.value = fwd.begin()
    this.end_.value = fwd.end()
  }

  func isEmpty() -> Bool {
    return begin_.value == end_.value
  }

  func begin() -> T {
    return begin_.value
  }

  func end() -> T {
    return end_.value
  }

  typealias EnumeratorType = ReverseRangeEnumerator<T>
  func getEnumeratorType() -> EnumeratorType {
    return EnumeratorType(begin_.value, end_.value)
  }

  var begin_, end_: GenericIVar<T>
}

// FIXME: make generic
// BLOCKED: <rdar://problem/12780068> Crash while trying to make iteration generic
struct IntEnumeratorType : Enumerator, Enumerable {
  typealias Element = Int
  var min : Int
  var max : Int
  var stride : Int

  // FIXME: each/reduce should be moved out to generic methods, or methods that
  // take the range as a protocol'd "enumeration/iterator" value.
  func each(f : (Int) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Int, f : (Int, Int) -> Int) -> Int {
    for i in this { val = f(val, i) }
    return val
  }

  func by(s : Int) -> IntEnumeratorType {
    var result = this
    result.stride = s
    return result
  }

  func isEmpty() -> Bool {
    return min >= max
  }
  func contains(x : Int) -> Bool {
    return min <= x && x < max
  }

  func next() -> Int {
    var prev = min
    min += stride
    return prev
  }

  typealias EnumeratorType = IntEnumeratorType
  func getEnumeratorType() -> IntEnumeratorType {
    return this
  }

  func replPrint() {
    print("\(min)..\(max)")
    if stride != 1 {
      print(" by \(stride)")
    }
  }
}

extension IntEnumeratorType : Equatable, Hashable {
  func __equal__(rhs: IntEnumeratorType) -> Bool {
    return min == rhs.min && max == rhs.max && stride == rhs.stride
  }
  func hashValue() -> Int {
    return min ^ max
  }
}

// FIXME: make generic
// BLOCKED: <rdar://problem/12780068> Crash while trying to make iteration generic
struct ReverseIntEnumeratorType : Enumerator, Enumerable {
  typealias Element = Int
  var min : Int
  var max : Int
  var stride : Int

  // FIXME: each/reduce should be moved out to generic methods, or methods that
  // take the range as a protocol'd "enumeration/iterator" value.
  func each(f : (Int) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Int, f : (Int, Int) -> Int) -> Int {
    for i in this { val = f(val, i) }
    return val
  }

  func isEmpty() -> Bool {
    return min >= max
  }
  func contains(x : Int) -> Bool {
    return min <= x && x < max
  }
  func next() -> Int {
    max -= stride
    return max
  }

  typealias EnumeratorType = ReverseIntEnumeratorType
  func getEnumeratorType() -> ReverseIntEnumeratorType {
    return this
  }

  func replPrint() {
    print("reverse(\(min)..\(max))")
    if stride != 1 {
      print(" by \(stride)")
    }
  }
}

extension ReverseIntEnumeratorType : Equatable, Hashable {
  func __equal__(rhs: ReverseIntEnumeratorType) -> Bool {
    return min == rhs.min && max == rhs.max && stride == rhs.stride
  }
  func hashValue() -> Int {
    return min ^ max
  }
}

func reverse<T: BidirectionalIndex>(rng: Range<T>) -> ReverseRange<T> {
  return ReverseRange(rng.begin(), rng.end())
}

func reverse<T: BidirectionalIndex>(rng: ReverseRange<T>) -> Range<T> {
  return Range(rng.begin(), rng.end())
}

func reverse(rng : IntEnumeratorType) -> ReverseIntEnumeratorType {
  return ReverseIntEnumeratorType(rng.min, rng.max, rng.stride)
}

func reverse(rng : ReverseIntEnumeratorType) -> IntEnumeratorType {
  return IntEnumeratorType(rng.min, rng.max, rng.stride)
}

func .. (min : Int, max : Int) -> IntEnumeratorType {
  return IntEnumeratorType(min, max, 1)
}

// FIXME: make generic
// BLOCKED: <rdar://problem/12780068> Crash while trying to make iteration generic
struct DoubleEnumeratorType : Enumerator, Enumerable {
  typealias Element = Double
  var min : Double,
  max : Double,
  stride : Double

  // FIXME: each/reduce should be moved out to generic methods, or methods that
  // take the range as a protocol'd "enumeration/iterator" value.
  func each(f : (Double) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Double, f : (Double, Double) -> Double) -> Double {
    for i in this { val = f(val, i) }
    return val
  }

  func by(s : Double) -> DoubleEnumeratorType {
    var result = this
    result.stride = s
    return result
  }

  func isEmpty() -> Bool {
    return min >= max
  }
  func next() -> Double {
    var prev = min
    min += stride
    return prev
  }

  typealias EnumeratorType = DoubleEnumeratorType
  func getEnumeratorType() -> DoubleEnumeratorType {
    return this
  }

  func replPrint() {
    print("\(min)..\(max)")
    if stride != 1.0 {
      print(" by \(stride)")
    }
  }
}

extension DoubleEnumeratorType : Equatable, Hashable {
  func __equal__(rhs: DoubleEnumeratorType) -> Bool {
    return min == rhs.min && max == rhs.max && stride == rhs.stride
  }
  func hashValue() -> Int {
    return min.hashValue() ^ max.hashValue()
  }
}

func .. (min : Double, max : Double) -> DoubleEnumeratorType {
  return DoubleEnumeratorType(min, max, 1.0)
}
