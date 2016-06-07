// RUN: %target-parse-verify-swift


func _Algorithm<I : IteratorProtocol, S : Sequence>(i: I, s: S) {
  func fn1(_: EnumerateGenerator<I>) {} // expected-error {{'EnumerateGenerator' has been renamed to 'EnumeratedIterator'}} {{15-33=EnumeratedIterator}} {{none}}
  func fn2(_: EnumerateSequence<S>) {} // expected-error {{'EnumerateSequence' has been renamed to 'EnumeratedSequence'}} {{15-32=EnumeratedSequence}} {{none}}
  _ = EnumeratedIterator(i) // expected-error {{use the 'enumerated()' method on the sequence}} {{none}}
  _ = EnumeratedSequence(s) // expected-error {{use the 'enumerated()' method on the sequence}} {{none}}
}

func _Arrays<T>(e: T) {
  // _ = ContiguousArray(count: 1, repeatedValue: e) // xpected-error {{Please use init(repeating:count:) instead}} {{none}}
  // _ = ArraySlice(count: 1, repeatedValue: e) // xpected-error {{Please use init(repeating:count:) instead}} {{none}}
  // _ = Array(count: 1, repeatedValue: e) // xpected-error {{Please use init(repeating:count:) instead}} {{none}}
  // The actual error is: {{argument 'repeatedValue' must precede argument 'count'}}

  var a = ContiguousArray<T>()
  _ = a.removeAtIndex(0) // expected-error {{'removeAtIndex' has been renamed to 'remove(at:)'}} {{9-22=remove}} {{23-23=at: }} {{none}}
  _ = a.replaceRange(0..<1, with: []) // expected-error {{'replaceRange(_:with:)' has been renamed to 'replaceSubrange(_:with:)'}} {{9-21=replaceSubrange}} {{none}}
  _ = a.appendContentsOf([]) // expected-error {{'appendContentsOf' has been renamed to 'append(contentsOf:)'}} {{9-25=append}} {{26-26=contentsOf: }} {{none}}

  var b = ArraySlice<T>()
  _ = b.removeAtIndex(0) // expected-error {{'removeAtIndex' has been renamed to 'remove(at:)'}} {{9-22=remove}} {{23-23=at: }} {{none}}
  _ = b.replaceRange(0..<1, with: []) // expected-error {{'replaceRange(_:with:)' has been renamed to 'replaceSubrange(_:with:)'}} {{9-21=replaceSubrange}} {{none}}
  _ = b.appendContentsOf([]) // expected-error {{'appendContentsOf' has been renamed to 'append(contentsOf:)'}} {{9-25=append}} {{26-26=contentsOf: }} {{none}}

  var c = Array<T>()
  _ = c.removeAtIndex(0) // expected-error {{'removeAtIndex' has been renamed to 'remove(at:)'}} {{9-22=remove}} {{23-23=at: }} {{none}}
  _ = c.replaceRange(0..<1, with: []) // expected-error {{'replaceRange(_:with:)' has been renamed to 'replaceSubrange(_:with:)'}} {{9-21=replaceSubrange}} {{none}}
  _ = c.appendContentsOf([]) // expected-error {{'appendContentsOf' has been renamed to 'append(contentsOf:)'}} {{9-25=append}} {{26-26=contentsOf: }} {{none}}
}

func _Builtin(o: AnyObject, oo: AnyObject?) {
  _ = unsafeAddressOf(o) // expected-error {{'unsafeAddressOf' has been renamed to 'unsafeAddress(of:)'}} {{7-22=unsafeAddress}} {{23-23=of: }} {{none}}
  _ = unsafeUnwrap(oo) // expected-error {{Removed in Swift 3. Please use Optional.unsafelyUnwrapped instead.}} {{none}}
}

func _CString() {
  _ = String.fromCString([]) // expected-error {{'fromCString' is unavailable: Please use String.init?(validatingUTF8:) instead. Note that it no longer accepts NULL as a valid input. Also consider using String(cString:), that will attempt to repair ill-formed code units.}} {{none}}
  _ = String.fromCStringRepairingIllFormedUTF8([]) // expected-error {{'fromCStringRepairingIllFormedUTF8' is unavailable: Please use String.init(cString:) instead. Note that it no longer accepts NULL as a valid input. See also String.decodeCString if you need more control.}} {{none}}
}

func _CTypes<T>(x: Unmanaged<T>) {
  func fn(_: COpaquePointer) {} // expected-error {{'COpaquePointer' has been renamed to 'OpaquePointer'}} {{14-28=OpaquePointer}} {{none}}
  _ = OpaquePointer(bitPattern: x) // expected-error {{'init(bitPattern:)' is unavailable: use 'Unmanaged.toOpaque()' instead}} {{none}}
}

func _ClosedRange(x: ClosedRange<Int>) {
  _ = x.startIndex // expected-error {{'startIndex' has been renamed to 'lowerBound'}} {{9-19=lowerBound}} {{none}}
  _ = x.endIndex // expected-error {{'endIndex' has been renamed to 'upperBound'}} {{9-17=upperBound}} {{none}}
}

func _Collection() {
  func fn(a: Bit) {} // expected-error {{'Bit' is unavailable: Bit enum has been removed. Please use Int instead.}} {{none}}
  func fn<T>(b: IndexingGenerator<T>) {} // expected-error {{'IndexingGenerator' has been renamed to 'IndexingIterator'}} {{17-34=IndexingIterator}} {{none}}
  func fn<T : CollectionType>(c: T) {} // expected-error {{'CollectionType' has been renamed to 'Collection'}} {{15-29=Collection}} {{none}}
  func fn<T>(d: PermutationGenerator<T, T>) {} // expected-error {{'PermutationGenerator' is unavailable: PermutationGenerator has been removed in Swift 3}}
}

func _Collection<C : Collection>(c: C) {
  func fn<T : Collection, U>(_: T, _: U) where T.Generator == U {} // expected-error {{'T' does not have a member type named 'Generator'; did you mean 'Iterator'?}} {{50-59=Iterator}} {{none}} 
  _ = c.generate() // expected-error {{'generate()' has been renamed to 'makeIterator()'}} {{9-17=makeIterator}} {{none}}
  _ = c.underestimateCount() // expected-error {{'underestimateCount()' is unavailable: Removed in Swift 3. Please use underestimatedCount property.}} {{none}}
  _ = c.split(1) { _ in return true} // expected-error {{split(maxSplits:omittingEmptySubsequences:whereSeparator:) instead}} {{none}}
}

func _Collection<C : Collection, E>(c: C, e: E) where C.Iterator.Element: Equatable, C.Iterator.Element == E {
  _ = c.split(e) // expected-error {{'split(_:maxSplit:allowEmptySlices:)' is unavailable: Please use split(separator:maxSplits:omittingEmptySubsequences:) instead}} {{none}}
}

func _CollectionAlgorithms<C : MutableCollection, I>(c: C, i: I) where C : RandomAccessCollection, C.Index == I {
  var c = c
  _ = c.partition(i..<i) { _, _ in true } // expected-error {{slice the collection using the range, and call partition(by:)}} {{none}}
  c.sortInPlace { _, _ in true } // expected-error {{'sortInPlace' has been renamed to 'sort(by:)'}} {{5-16=sort}} {{none}}
}

func _CollectionAlgorithms<C : MutableCollection, I>(c: C, i: I) where C : RandomAccessCollection, C.Iterator.Element : Comparable, C.Index == I {
  var c = c
  _ = c.partition(i..<i) // expected-error {{slice the collection using the range, and call partition()}} {{none}}
  c.sortInPlace() // expected-error {{'sortInPlace()' has been renamed to 'sort()'}} {{5-16=sort}} {{none}}
}

func _CollectionAlgorithms<C : Collection, E>(c: C, e: E) where C.Iterator.Element : Equatable, C.Iterator.Element == E {
  _ = c.indexOf(e)  // expected-error {{'indexOf' has been renamed to 'index(of:)'}} {{9-16=index}} {{17-17=of: }} {{none}}
}

func _CollectionAlgorithms<C : Collection>(c: C) {
  _ = c.indexOf { _ in true } // expected-error {{'indexOf' has been renamed to 'index(where:)'}} {{9-16=index}} {{none}}
}

func _CollectionOfOne<T>(i: IteratorOverOne<T>) {
  func fn(_: GeneratorOfOne<T>) {} // expected-error {{'GeneratorOfOne' has been renamed to 'IteratorOverOne'}} {{14-28=IteratorOverOne}} {{none}}
  _ = i.generate() // expected-error {{'generate()' has been renamed to 'makeIterator()'}} {{9-17=makeIterator}} {{none}}
}

func _CompilerProtocols() {
  func fn(_: BooleanType) {} // expected-error {{'BooleanType' has been renamed to 'Boolean'}} {{14-25=Boolean}} {{none}}
}

func _EmptyCollection<T>(i: EmptyIterator<T>) {
  func fn(_: EmptyGenerator<T>) {} // expected-error {{'EmptyGenerator' has been renamed to 'EmptyIterator'}} {{14-28=EmptyIterator}} {{none}}
  _ = i.generate() // expected-error {{'generate()' has been renamed to 'makeIterator()'}} {{9-17=makeIterator}} {{none}}
}

func _ErrorType() {
  func fn(_: ErrorType) {} // expected-error {{'ErrorType' has been renamed to 'Error'}} {{14-23=Error}}
}

func _ExistentialCollection<T>(i: AnyIterator<T>) {
  func fn1<T>(_: AnyGenerator<T>) {} // expected-error {{'AnyGenerator' has been renamed to 'AnyIterator'}} {{18-30=AnyIterator}} {{none}}
  func fn2<T : AnyCollectionType>(_: T) {} // expected-error {{'AnyCollectionType' has been renamed to 'AnyCollectionProtocol'}} {{16-33=AnyCollectionProtocol}} {{none}}
  func fn3(_: AnyForwardIndex) {} // expected-error {{'AnyForwardIndex' has been renamed to 'AnyIndex'}} {{15-30=AnyIndex}} {{none}}
  func fn4(_: AnyBidirectionalIndex) {} // expected-error {{'AnyBidirectionalIndex' has been renamed to 'AnyIndex'}} {{15-36=AnyIndex}} {{none}}
  func fn5(_: AnyRandomAccessIndex) {} // expected-error {{'AnyRandomAccessIndex' has been renamed to 'AnyIndex'}} {{15-35=AnyIndex}} {{none}}

  _ = anyGenerator(i) // expected-error {{'anyGenerator' has been replaced by 'AnyIterator.init(_:)'}} {{7-19=AnyIterator}} {{none}}
  _ = anyGenerator { i.next() } // expected-error {{'anyGenerator' has been replaced by 'AnyIterator.init(_:)'}} {{7-19=AnyIterator}} {{none}}
}
func _ExistentialCollection<T>(s: AnySequence<T>) {
  _ = s.underestimateCount() // expected-error {{'underestimateCount()' is unavailable: Please use underestimatedCount property instead.}} {{none}}
}
func _ExistentialCollection<T>(c: AnyCollection<T>) {
  _ = c.underestimateCount() // expected-error {{'underestimateCount()' is unavailable: Please use underestimatedCount property instead.}} {{none}}
}
func _ExistentialCollection<T>(c: AnyBidirectionalCollection<T>) {
  _ = c.underestimateCount() // expected-error {{'underestimateCount()' is unavailable: Please use underestimatedCount property instead.}} {{none}}
}
func _ExistentialCollection<T>(c: AnyRandomAccessCollection<T>) {
  _ = c.underestimateCount() // expected-error {{'underestimateCount()' is unavailable: Please use underestimatedCount property instead.}} {{none}}
}
func _ExistentialCollection<C : AnyCollectionProtocol>(c: C) {
  _ = c.generate() // expected-error {{'generate()' has been renamed to 'makeIterator()'}} {{9-17=makeIterator}} {{none}}
}

func _Filter() {
  func fn<T>(_: LazyFilterGenerator<T>) {} // expected-error {{'LazyFilterGenerator' has been renamed to 'LazyFilterIterator'}} {{17-36=LazyFilterIterator}} {{none}}
}
func _Filter<I : IteratorProtocol>(i: I) {
  _ = LazyFilterIterator(i) { _ in true } // expected-error {{'init(_:whereElementsSatisfy:)' is unavailable: use '.lazy.filter' on the sequence}}
}
func _Filter<S : Sequence>(s: S) {
  _ = LazyFilterSequence(s) { _ in true } // expected-error {{'init(_:whereElementsSatisfy:)' is unavailable: use '.lazy.filter' on the sequence}}
}
func _Filter<S>(s: LazyFilterSequence<S>) {
  _ = s.generate() // expected-error {{'generate()' has been renamed to 'makeIterator()'}} {{9-17=makeIterator}} {{none}}
}
func _Filter<C : Collection>(c: C) {
  _ = LazyFilterCollection(c) { _ in true} // expected-error {{'init(_:whereElementsSatisfy:)' is unavailable: use '.lazy.filter' on the collection}}
}
func _Filter<C>(c: LazyFilterCollection<C>) {
  _ = c.generate() // expected-error {{'generate()' has been renamed to 'makeIterator()'}} {{9-17=makeIterator}} {{none}}
}

func _FixedPoint() {
  var i: Int = 0
  var u: UInt = 0
  i++ // expected-error {{'++' is unavailable: it has been removed in Swift 3}} {{4-6= += 1}} {{none}}
  ++i // expected-error {{'++' is unavailable: it has been removed in Swift 3}} {{3-5=}} {{6-6= += 1}} {{none}}
  i-- // expected-error {{'--' is unavailable: it has been removed in Swift 3}} {{4-6= -= 1}} {{none}}
  --i // expected-error {{'--' is unavailable: it has been removed in Swift 3}} {{3-5=}} {{6-6= -= 1}} {{none}}
  u++ // expected-error {{'++' is unavailable: it has been removed in Swift 3}} {{4-6= += 1}} {{none}}
  ++u // expected-error {{'++' is unavailable: it has been removed in Swift 3}} {{3-5=}} {{6-6= += 1}} {{none}}
  u-- // expected-error {{'--' is unavailable: it has been removed in Swift 3}} {{4-6= -= 1}} {{none}}
  --u // expected-error {{'--' is unavailable: it has been removed in Swift 3}} {{3-5=}} {{6-6= -= 1}} {{none}}

  func fn1<T: IntegerType>(i: T) {} // expected-error {{'IntegerType' has been renamed to 'Integer'}} {{15-26=Integer}} {{none}}
  func fn2<T: SignedIntegerType>(i: T) {} // expected-error {{'SignedIntegerType' has been renamed to 'SignedInteger'}} {{15-32=SignedInteger}} {{none}}
  func fn3<T: UnsignedIntegerType>(i: T) {} // expected-error {{'UnsignedIntegerType' has been renamed to 'UnsignedInteger'}} {{15-34=UnsignedInteger}} {{none}}
}

func _Flatten() {
  func fn<T>(i: FlattenGenerator<T>) {} // expected-error {{'FlattenGenerator' has been renamed to 'FlattenIterator'}} {{17-33=FlattenIterator}} {{none}}
}
func _Flatten<T>(s: FlattenSequence<T>) {
  _ = s.generate() // expected-error {{'generate()' has been renamed to 'makeIterator()'}} {{9-17=makeIterator}} {{none}}
}
func _Flatten<T>(c: FlattenCollection<T>) {
  _ = c.underestimateCount() // expected-error {{'underestimateCount()' is unavailable: Please use underestimatedCount property instead.}} {{none}}
}
func _Flatten<T>(c: FlattenBidirectionalCollection<T>) {
  _ = c.underestimateCount() // expected-error {{'underestimateCount()' is unavailable: Please use underestimatedCount property instead.}} {{none}}
}

func _FloatingPoint() {
  func fn<F : FloatingPointType>(f: F) {} // expected-error {{'FloatingPointType' has been renamed to 'FloatingPoint'}} {{15-32=FloatingPoint}} {{none}}
}
func _FloatingPoint<F : BinaryFloatingPoint>(f: F) {
  _ = f.isSignaling // expected-error {{'isSignaling' has been renamed to 'isSignalingNaN'}} {{9-20=isSignalingNaN}} {{none}}
}

func _FloatingPointTypes() {
  var x: Float = 1, y: Float = 1
  // FIXME: isSignMinus -> sign is OK? different type.
  _ = x.isSignMinus // expected-error {{'isSignMinus' has been renamed to 'sign'}} {{9-20=sign}} {{none}}
  _ = x % y // expected-error {{'%' is unavailable: Use truncatingRemainder instead}} {{none}}
  x %= y // expected-error {{'%=' is unavailable: Use formTruncatingRemainder instead}} {{none}}
  ++x // expected-error {{'++' is unavailable: it has been removed in Swift 3}} {{3-5=}} {{6-6= += 1}} {{none}}
  --x // expected-error {{'--' is unavailable: it has been removed in Swift 3}} {{3-5=}} {{6-6= -= 1}} {{none}}
  x++ // expected-error {{'++' is unavailable: it has been removed in Swift 3}} {{4-6= += 1}} {{none}}
  x-- // expected-error {{'--' is unavailable: it has been removed in Swift 3}} {{4-6= -= 1}} {{none}}
}

func _HashedCollection<T>(x: Set<T>, i: Set<T>.Index, e: T) {
  var x = x
  _ = x.removeAtIndex(i) // expected-error {{'removeAtIndex' has been renamed to 'remove(at:)'}} {{9-22=remove}} {{23-23=at: }} {{none}}
  _ = x.generate() // expected-error {{'generate()' has been renamed to 'makeIterator()'}} {{9-17=makeIterator}} {{none}}
  _ = x.indexOf(e) // expected-error {{'indexOf' has been renamed to 'index(of:)'}} {{9-16=index}} {{17-17=of: }} {{none}}
}
func _HashedCollection<K, V>(x: Dictionary<K, V>, i: Dictionary<K, V>.Index, k: K) {
  var x = x
  _ = x.removeAtIndex(i) // expected-error {{'removeAtIndex' has been renamed to 'remove(at:)'}} {{9-22=remove}} {{23-23=at: }} {{none}}
  _ = x.indexForKey(k) // expected-error {{'indexForKey' has been renamed to 'index(forKey:)'}} {{9-20=index}} {{21-21=forKey: }} {{none}}
  _ = x.removeValueForKey(k) // expected-error {{'removeValueForKey' has been renamed to 'removeValue(forKey:)'}} {{9-26=removeValue}} {{27-27=forKey: }} {{none}}
  _ = x.generate() // expected-error {{'generate()' has been renamed to 'makeIterator()'}} {{9-17=makeIterator}} {{none}}
}

func _ImplicitlyUnwrappedOptional<T>(x: ImplicitlyUnwrappedOptional<T>) {
  _ = ImplicitlyUnwrappedOptional<T>() // expected-error {{'init()' is unavailable: Please use nil literal instead.}} {{none}}
  try! _ = ImplicitlyUnwrappedOptional<T>.map(x)() { _ in true } // expected-error {{'map' is unavailable: Has been removed in Swift 3.}}
  try! _ = ImplicitlyUnwrappedOptional<T>.flatMap(x)() { _ in true } // expected-error {{'flatMap' is unavailable: Has been removed in Swift 3.}}
  // FIXME: No way to call map and flatMap as method?
  // _ = (x as ImplicitlyUnwrappedOptional).map { _ in true } // xpected-error {{}} {{none}}
  // _ = (x as ImplicitlyUnwrappedOptional).flatMap { _ in true } // xpected-error {{}} {{none}}
}

func _Index<T : _Incrementable>(i: T) {
  var i = i
  --i // expected-error {{'--' is unavailable: it has been removed in Swift 3}} {{3-5=}} {{6-6= = i.predecessor()}} {{none}}
  i-- // expected-error {{'--' is unavailable: it has been removed in Swift 3}} {{4-6= = i.predecessor()}} {{none}}
  ++i // expected-error {{'++' is unavailable: it has been removed in Swift 3}} {{3-5=}} {{6-6= = i.successor()}} {{none}}
  i++ // expected-error {{'++' is unavailable: it has been removed in Swift 3}} {{4-6= = i.successor()}} {{none}}
}

func _Index() {
  func fn1<T : ForwardIndexType>(_: T) {} // expected-error {{'ForwardIndexType' has been renamed to 'Comparable'}} {{16-32=Comparable}} {{none}}
  func fn2<T : BidirectionalIndexType>(_: T) {} // expected-error {{'BidirectionalIndexType' has been renamed to 'Comparable'}} {{16-38=Comparable}} {{none}}
  func fn3<T : RandomAccessIndexType>(_: T) {} // expected-error {{'RandomAccessIndexType' has been renamed to 'Strideable'}} {{16-37=Strideable}} {{none}}
}

func _IntegerArithmetic() {
  func fn1<T : IntegerArithmeticType>(_: T) {} // expected-error {{'IntegerArithmeticType' has been renamed to 'IntegerArithmetic'}} {{16-37=IntegerArithmetic}} {{none}}
  func fn2<T : SignedNumberType>(_: T) {} // expected-error {{'SignedNumberType' has been renamed to 'SignedNumber'}} {{16-32=SignedNumber}} {{none}}
}

func _Join() {
  func fn<T>(_: JoinGenerator<T>) {} // expected-error {{'JoinGenerator' has been renamed to 'JoinedIterator'}} {{17-30=JoinedIterator}} {{none}}
}
func _Join<T>(s: JoinedSequence<T>) {
  _ = s.generate() // expected-error {{'generate()' has been renamed to 'makeIterator()'}} {{9-17=makeIterator}} {{none}}
}
func _Join<S : Sequence>(s: S) where S.Iterator.Element : Sequence {
  _ = s.joinWithSeparator(s) // expected-error {{'joinWithSeparator' has been renamed to 'joined(separator:)'}} {{9-26=joined}} {{27-27=separator: }} {{none}}
}

func _LazyCollection() {
  func fn<T : LazyCollectionType>(_: T) {} // expected-error {{'LazyCollectionType' has been renamed to 'LazyCollectionProtocol'}} {{15-33=LazyCollectionProtocol}} {{none}}
}

func _LazySequence() {
  func fn<T : LazySequenceType>(_: T) {} // expected-error {{'LazySequenceType' has been renamed to 'LazySequenceProtocol'}} {{15-31=LazySequenceProtocol}} {{none}}
}
func _LazySequence<S : LazySequenceProtocol>(s: S) {
  _ = s.array // expected-error {{'array' is unavailable: Please use Array initializer instead.}} {{none}}
}

func _ManagedBuffer<V, E>(x: ManagedBufferPointer<V, E>) {
  _ = x.allocatedElementCount // expected-error {{'allocatedElementCount' has been renamed to 'capacity'}} {{9-30=capacity}} {{none}}
}

func _Map() {
  func fn<B, E>(_: LazyMapGenerator<B, E>) {} // expected-error {{'LazyMapGenerator' has been renamed to 'LazyMapIterator'}} {{20-36=LazyMapIterator}} {{none}}
}
func _Map<S : Sequence>(s: S) {
  _ = LazyMapSequence(s) { _ in true } // expected-error {{'init(_:transform:)' is unavailable: use '.lazy.map' on the sequence}} {{none}}
}
func _Map<C : Collection>(c: C) {
  _ = LazyMapCollection(c) { _ in true } // expected-error {{'init(_:transform:)' is unavailable: use '.lazy.map' on the collection}} {{none}}
}

func _Mirror() {
  func fn<M : MirrorPathType>(_: M) {} // expected-error {{'MirrorPathType' has been renamed to 'MirrorPath'}} {{15-29=MirrorPath}} {{none}}
}

func _MutableCollection() {
  func fn1<C : MutableCollectionType>(_: C) {} // expected-error {{'MutableCollectionType' has been renamed to 'MutableCollection'}} {{16-37=MutableCollection}} {{none}}
  func fn2<C : MutableSliceable>(_: C) {} // expected-error {{'MutableSliceable' is unavailable: Please use 'Collection where SubSequence : MutableCollection'}} {{none}}
}

func _OptionSet() {
  func fn<O : OptionSetType>(_: O) {} // expected-error {{'OptionSetType' has been renamed to 'OptionSet'}} {{15-28=OptionSet}} {{none}}
}

func _Optional<T>(x: T) {
  _ = Optional<T>.None // expected-error {{'None' has been renamed to 'none'}} {{19-23=none}} {{none}}
  _ = Optional<T>.Some(x) // expected-error {{'Some' has been renamed to 'some'}} {{19-23=some}} {{none}}
}

func _OutputStream() {
  func fn<S : OutputStreamType>(_: S) {} // expected-error {{'OutputStreamType' has been renamed to 'OutputStream'}} {{15-31=OutputStream}} {{none}}
}
func _OutputStream<S : Streamable, O : OutputStream>(s: S, o: O) {
  var o = o
  s.writeTo(&o) // expected-error {{'writeTo' has been renamed to 'write(to:)'}} {{5-12=write}} {{13-13=to: }} {{none}}
}

func _Policy() {
  func fn<O : BitwiseOperationsType>(_: O) {} // expected-error {{'BitwiseOperationsType' has been renamed to 'BitwiseOperations'}} {{15-36=BitwiseOperations}} {{none}}
}

func _Print<T>(x: T) {
  print(x, appendNewline: true) // expected-error {{'print(_:appendNewline:)' is unavailable: Please use 'terminator: ""' instead of 'appendNewline: false': 'print((...), terminator: "")'}} {{none}}
  debugPrint(x, appendNewline: true) // expected-error {{'debugPrint(_:appendNewline:)' is unavailable: Please use 'terminator: ""' instead of 'appendNewline: false': 'debugPrint((...), terminator: "")'}} {{none}}
}

func _Print<T, O : OutputStream>(x: T, o: O) {
  // FIXME: Not working due to <rdar://22101775>
  //var o = o
  //print(x, &o) // xpected-error {{}} {{none}}
  //debugPrint(x, &o) // xpected-error {{}} {{none}}
  //print(x, &o, appendNewline: true) // xpected-error {{}} {{none}}
  //debugPrint(x, &o, appendNewline: true) // xpected-error {{}} {{none}}
}

func _Range() {
  func fn1<B>(_: RangeGenerator<B>) {} // expected-error {{'RangeGenerator' has been renamed to 'IndexingIterator'}} {{18-32=IndexingIterator}} {{none}}
  func fn2<I : IntervalType>(_: I) {} // expected-error {{'IntervalType' is unavailable: IntervalType has been removed in Swift 3. Use ranges instead.}} {{none}}
  func fn3<B>(_: HalfOpenInterval<B>) {} // expected-error {{'HalfOpenInterval' has been renamed to 'Range'}} {{18-34=Range}} {{none}}
  func fn4<B>(_: ClosedInterval<B>) {} // expected-error {{'ClosedInterval' has been renamed to 'ClosedRange'}} {{18-32=ClosedRange}} {{none}}
}
func _Range<T>(r: Range<T>) {
  _ = r.startIndex // expected-error {{'startIndex' has been renamed to 'lowerBound'}} {{9-19=lowerBound}} {{none}}
  _ = r.endIndex // expected-error {{'endIndex' has been renamed to 'upperBound'}} {{9-17=upperBound}} {{none}}
}
func _Range<T>(r: ClosedRange<T>) {
  _ = r.clamp(r) // expected-error {{'clamp' is unavailable: Call clamped(to:) and swap the argument and the receiver.  For example, x.clamp(y) becomes y.clamped(to: x) in Swift 3.}} {{none}}
}
func _Range<T>(r: CountableClosedRange<T>) {
  _ = r.clamp(r) // expected-error {{'clamp' is unavailable: Call clamped(to:) and swap the argument and the receiver.  For example, x.clamp(y) becomes y.clamped(to: x) in Swift 3.}} {{none}}
}

func _RangeReplaceableCollection() {
  func fn<I : RangeReplaceableCollectionType>(_: I) {} // expected-error {{'RangeReplaceableCollectionType' has been renamed to 'RangeReplaceableCollection'}} {{15-45=RangeReplaceableCollection}} {{none}}
}
func _RangeReplaceableCollection<C : RangeReplaceableCollection>(c: C, i: C.Index) {
  var c = c
  c.replaceRange(i..<i, with: []) // expected-error {{'replaceRange(_:with:)' has been renamed to 'replaceSubrange(_:with:)'}} {{5-17=replaceSubrange}} {{none}}
  _ = c.removeAtIndex(i) // expected-error {{'removeAtIndex' has been renamed to 'remove(at:)'}} {{9-22=remove}} {{23-23=at: }} {{none}}
  c.removeRange(i..<i) // expected-error {{'removeRange' has been renamed to 'removeSubrange'}} {{5-16=removeSubrange}} {{none}}
  c.appendContentsOf([]) // expected-error {{'appendContentsOf' has been renamed to 'append(contentsOf:)'}} {{5-21=append}} {{22-22=contentsOf: }} {{none}}
  c.insertContentsOf(c, at: i) // expected-error {{'insertContentsOf(_:at:)' has been renamed to 'insert(contentsOf:at:)'}} {{5-21=insert}} {{22-22=contentsOf: }} {{none}}
}

func _Reflection(x: ObjectIdentifier) {
  _ = x.uintValue // expected-error {{'uintValue' is unavailable: use the 'UInt(_:)' initializer}} {{none}}
}

func _Repeat() {
  func fn<E>(_: Repeat<E>) {} // expected-error {{'Repeat' has been renamed to 'Repeated'}} {{17-23=Repeated}} {{none}}
}
func _Repeat<E>(e: E) {
  _ = Repeated(count: 0, repeatedValue: e) // expected-error {{'init(count:repeatedValue:)' is unavailable: Please use repeatElement(_:count:) function instead}} {{none}}
}

func _Reverse<C : BidirectionalCollection>(c: C) {
  _ = ReversedCollection(c) // expected-error {{'init' is unavailable: use the 'reversed()' method on the collection}} {{none}}
  _ = c.reverse() // expected-error {{'reverse()' has been renamed to 'reversed()'}} {{9-16=reversed}} {{none}}
}
func _Reverse<C : RandomAccessCollection>(c: C) {
  _ = ReversedRandomAccessCollection(c) // expected-error {{'init' is unavailable: use the 'reversed()' method on the collection}} {{none}}
  _ = c.reverse() // expected-error {{'reverse()' has been renamed to 'reversed()'}} {{9-16=reversed}} {{none}}
}
func _Reverse<C : LazyCollectionProtocol>(c: C) where C : BidirectionalCollection, C.Elements : BidirectionalCollection {
  _ = c.reverse() // expected-error {{'reverse()' has been renamed to 'reversed()'}} {{9-16=reversed}} {{none}}
}
func _Reverse<C : LazyCollectionProtocol>(c: C) where C : RandomAccessCollection, C.Elements : RandomAccessCollection {
  _ = c.reverse() // expected-error {{'reverse()' has been renamed to 'reversed()'}} {{9-16=reversed}} {{none}}
}

func _Sequence() {
  func fn1<G : GeneratorType>(_: G) {} // expected-error {{'GeneratorType' has been renamed to 'IteratorProtocol'}} {{16-29=IteratorProtocol}} {{none}}
  func fn2<S : SequenceType>(_: S) {} // expected-error {{'SequenceType' has been renamed to 'Sequence'}} {{16-28=Sequence}} {{none}}
  func fn3<I : IteratorProtocol>(_: GeneratorSequence<I>) {} // expected-error {{'GeneratorSequence' has been renamed to 'IteratorSequence'}} {{37-54=IteratorSequence}} {{none}}
}
func _Sequence<S : Sequence>(s: S) {
  _ = s.generate() // expected-error {{'generate()' has been renamed to 'makeIterator()'}} {{9-17=makeIterator}} {{none}}
  _ = s.underestimateCount() // expected-error {{'underestimateCount()' is unavailable: it became a property 'underestimatedCount'}} {{none}}
  _ = s.split(1, allowEmptySlices: true) { _ in true } // expected-error {{'split(_:allowEmptySlices:isSeparator:)' is unavailable: call 'split(maxSplits:omittingEmptySubsequences:isSeparator:)' and invert the 'allowEmptySlices' argument}} {{none}}
}
func _Sequence<S : Sequence>(s: S, e: S.Iterator.Element) where S.Iterator.Element : Equatable {
  _ = s.split(e, maxSplit: 1, allowEmptySlices: true) // expected-error {{'split(_:maxSplit:allowEmptySlices:)' is unavailable: call 'split(separator:maxSplits:omittingEmptySubsequences:)' and invert the 'allowEmptySlices' argument}} {{none}}
}

func _SequenceAlgorithms<S : Sequence>(x: S) {
  _ = x.enumerate() // expected-error {{'enumerate()' has been renamed to 'enumerated()'}} {{9-18=enumerated}} {{none}}
  _ = x.minElement { _, _ in true } // expected-error {{'minElement' has been renamed to 'min(by:)'}} {{9-19=min}} {{none}}
  _ = x.maxElement { _, _ in true } // expected-error {{'maxElement' has been renamed to 'max(by:)'}} {{9-19=max}} {{none}}
  _ = x.reverse() // expected-error {{'reverse()' has been renamed to 'reversed()'}} {{9-16=reversed}} {{none}}
  _ = x.startsWith([]) { _ in true } // expected-error {{'startsWith(_:isEquivalent:)' has been renamed to 'starts(with:by:)'}} {{9-19=starts}} {{20-20=with: }} {{none}}
  _ = x.lexicographicalCompare([]) { _, _ in true } // expected-error {{'lexicographicalCompare(_:isOrderedBefore:)' has been renamed to 'lexicographicallyPrecedes(_:by:)'}} {{9-31=lexicographicallyPrecedes}}{{none}}
}
func _SequenceAlgorithms<S : Sequence>(x: S) where S.Iterator.Element : Comparable {
  _ = x.minElement() // expected-error {{'minElement()' has been renamed to 'min()'}} {{9-19=min}} {{none}}
  _ = x.maxElement() // expected-error {{'maxElement()' has been renamed to 'max()'}} {{9-19=max}} {{none}}
  _ = x.startsWith([]) // expected-error {{'startsWith' has been renamed to 'starts(with:)'}} {{9-19=starts}} {{20-20=with: }} {{none}}
  _ = x.lexicographicalCompare([]) // expected-error {{'lexicographicalCompare' has been renamed to 'lexicographicallyPrecedes'}} {{9-31=lexicographicallyPrecedes}}{{none}}
}

func _SetAlgebra() {
  func fn<S : SetAlgebraType>(_: S) {} // expected-error {{'SetAlgebraType' has been renamed to 'SetAlgebra'}} {{15-29=SetAlgebra}} {{none}}
}
func _SetAlgebra<S : SetAlgebra>(s: S) {
  var s = s
  _ = s.intersect(s) // expected-error {{'intersect' has been renamed to 'intersection(_:)'}} {{9-18=intersection}} {{none}}
  _ = s.exclusiveOr(s) // expected-error {{'exclusiveOr' has been renamed to 'symmetricDifference(_:)'}} {{9-20=symmetricDifference}} {{none}}
  s.unionInPlace(s) // expected-error {{'unionInPlace' has been renamed to 'formUnion(_:)'}} {{5-17=formUnion}} {{none}}
  s.intersectInPlace(s) // expected-error {{'intersectInPlace' has been renamed to 'formIntersection(_:)'}} {{5-21=formIntersection}} {{none}}
  s.exclusiveOrInPlace(s) // expected-error {{'exclusiveOrInPlace' has been renamed to 'formSymmetricDifference(_:)'}} {{5-23=formSymmetricDifference}} {{none}}
  _ = s.isSubsetOf(s) // expected-error {{'isSubsetOf' has been renamed to 'isSubset(of:)'}} {{9-19=isSubset}} {{20-20=of: }} {{none}}
  _ = s.isDisjointWith(s) // expected-error {{'isDisjointWith' has been renamed to 'isDisjoint(with:)'}} {{9-23=isDisjoint}} {{24-24=with: }} {{none}}
  s.subtractInPlace(s) // expected-error {{'subtractInPlace' has been renamed to 'subtract(_:)'}} {{5-20=subtract}} {{none}}
  _ = s.isStrictSupersetOf(s) // expected-error {{'isStrictSupersetOf' has been renamed to 'isStrictSuperset(of:)'}} {{9-27=isStrictSuperset}} {{28-28=of: }} {{none}}
  _ = s.isStrictSubsetOf(s) // expected-error {{'isStrictSubsetOf' has been renamed to 'isStrictSubset(of:)'}} {{9-25=isStrictSubset}} {{26-26=of: }} {{none}}
}

func _StaticString(x: StaticString) {
  _ = x.byteSize // expected-error {{'byteSize' has been renamed to 'utf8CodeUnitCount'}} {{9-17=utf8CodeUnitCount}} {{none}}
  _ = x.stringValue // expected-error {{'stringValue' is unavailable: use the 'String(_:)' initializer}} {{none}}
}

func _Stride<T : Strideable>(x: T, d: T.Stride) {
  func fn1<T>(_: StrideToGenerator<T>) {} // expected-error {{'StrideToGenerator' has been renamed to 'StrideToIterator'}} {{18-35=StrideToIterator}} {{none}}
  func fn2<T>(_: StrideThroughGenerator<T>) {} // expected-error {{'StrideThroughGenerator' has been renamed to 'StrideThroughIterator'}} {{18-40=StrideThroughIterator}} {{none}}

  _ = x.stride(to: x, by: d) // expected-error {{'stride(to:by:)' is unavailable: Use stride(from:to:by:) free function instead}} {{none}}
  _ = x.stride(through: x, by: d) // expected-error {{'stride(through:by:)' is unavailable: Use stride(from:through:by:) free function instead}}
}

func _String<S, C>(x: String, s: S, c: C, i: String.Index)
  where S : Sequence, S.Iterator.Element == Character, C : Collection, C.Iterator.Element == Character {
  var x = x
  x.appendContentsOf(x) // expected-error {{'appendContentsOf' has been renamed to 'append(_:)'}} {{5-21=append}} {{none}}
  x.appendContentsOf(s) // expected-error {{'appendContentsOf' has been renamed to 'append(contentsOf:)'}} {{5-21=append}} {{22-22=contentsOf: }} {{none}}
  x.insertContentsOf(c, at: i) // expected-error {{'insertContentsOf(_:at:)' has been renamed to 'insert(contentsOf:at:)'}} {{5-21=insert}} {{22-22=contentsOf: }} {{none}}
  x.replaceRange(i..<i, with: c) // expected-error {{'replaceRange(_:with:)' has been renamed to 'replaceSubrange'}} {{5-17=replaceSubrange}} {{none}}
  x.replaceRange(i..<i, with: x) // expected-error {{'replaceRange(_:with:)' has been renamed to 'replaceSubrange'}} {{5-17=replaceSubrange}} {{none}}
  _ = x.removeAtIndex(i) // expected-error {{'removeAtIndex' has been renamed to 'remove(at:)'}} {{9-22=remove}} {{23-23=at: }} {{none}}
  x.removeRange(i..<i) // expected-error {{'removeRange' has been renamed to 'removeSubrange'}} {{5-16=removeSubrange}} {{none}}
  _ = x.lowercaseString // expected-error {{'lowercaseString' has been renamed to 'lowercased()'}} {{9-24=lowercased}} {{none}}
  _ = x.uppercaseString // expected-error {{'uppercaseString' has been renamed to 'uppercased()'}} {{9-24=uppercased}} {{none}}
  // FIXME: SR-1649 <rdar://problem/26563343>; We should suggest to add '()'
}
func _String<S : Sequence>(s: S, sep: String) where S.Iterator.Element == String {
  _ = s.joinWithSeparator(sep) // expected-error {{'joinWithSeparator' has been renamed to 'joined(separator:)'}} {{9-26=joined}} {{27-27=separator: }} {{none}}
}

func _StringCharacterView<S, C>(x: String.CharacterView, s: S, c: C, i: String.CharacterView.Index)
  where S : Sequence, S.Iterator.Element == Character, C : Collection, C.Iterator.Element == Character {
  var x = x
  x.replaceRange(i..<i, with: c) // expected-error {{'replaceRange(_:with:)' has been renamed to 'replaceSubrange'}} {{5-17=replaceSubrange}} {{none}}
  x.appendContentsOf(s) // expected-error {{'appendContentsOf' has been renamed to 'append(contentsOf:)'}} {{5-21=append}} {{22-22=contentsOf: }} {{none}}
}

func _StringLegacy(c: Character, u: UnicodeScalar) {
  _ = String(count: 1, repeatedValue: c) // expected-error {{'init(count:repeatedValue:)' is unavailable: Renamed to init(repeating:count:) and reordered parameters}} {{none}}
  _ = String(count: 1, repeatedValue: u) // expected-error {{'init(count:repeatedValue:)' is unavailable: Renamed to init(repeating:count:) and reordered parameters}} {{none}}
}

func _Unicode() {
  func fn<T : UnicodeCodecType>(_: T) {} // expected-error {{'UnicodeCodecType' has been renamed to 'UnicodeCodec'}} {{15-31=UnicodeCodec}} {{none}}
}
func _Unicode<I : IteratorProtocol, E : UnicodeCodec>(i: I, e: E.Type) where I.Element == E.CodeUnit {
  _ = transcode(e, e, i, { _ in }, stopOnError: true) // expected-error {{'transcode(_:_:_:_:stopOnError:)' is unavailable: use 'transcode(_:from:to:stoppingOnError:sendingOutputTo:)'}} {{none}}
  _ = UTF16.measure(e, input: i, repairIllFormedSequences: true) // expected-error {{'measure(_:input:repairIllFormedSequences:)' is unavailable: use 'transcodedLength(of:decodedAs:repairingIllFormedSequences:)'}} {{none}}
}

func _UnicodeScalar(s: UnicodeScalar) {
  _ = UnicodeScalar() // expected-error {{'init()' is unavailable: use 'UnicodeScalar(0)'}} {{none}}
  _ = s.escape(asASCII: true) // expected-error {{'escape(asASCII:)' has been renamed to 'escaped(asASCII:)'}} {{9-15=escaped}} {{none}}
}

func _Unmanaged<T>(x: Unmanaged<T>, p: OpaquePointer) {
  _ = Unmanaged<T>.fromOpaque(p) // expected-error {{'fromOpaque' is unavailable: use 'fromOpaque(_: UnsafePointer<Void>)' instead}} {{none}}
  let _: OpaquePointer = x.toOpaque() // expected-error {{'toOpaque()' is unavailable: use 'toOpaque() -> UnsafePointer<Void>' instead}} {{none}}
}

func _UnsafeBufferPointer() {
  func fn<T>(x: UnsafeBufferPointerGenerator<T>) {} // expected-error {{'UnsafeBufferPointerGenerator' has been renamed to 'UnsafeBufferPointerIterator'}} {{17-45=UnsafeBufferPointerIterator}} {{none}}
}

func _UnsafePointer<T>(x: UnsafePointer<T>) {
  _ = UnsafePointer<T>.Memory.self // expected-error {{'Memory' has been renamed to 'Pointee'}} {{24-30=Pointee}} {{none}}
  _ = UnsafePointer<T>() // expected-error {{'init()' is unavailable: use 'nil' literal}} {{none}}
  _ = x.memory // expected-error {{'memory' has been renamed to 'pointee'}} {{9-15=pointee}} {{none}}
}

func _UnsafePointer<T>(x: UnsafeMutablePointer<T>, e: T) {
  var x = x
  _ = UnsafeMutablePointer<T>.Memory.self // expected-error {{'Memory' has been renamed to 'Pointee'}} {{31-37=Pointee}} {{none}}
  _ = UnsafeMutablePointer<T>() // expected-error {{'init()' is unavailable: use 'nil' literal}} {{none}}
  _ = x.memory // expected-error {{'memory' has been renamed to 'pointee'}} {{9-15=pointee}} {{none}}
  _ = UnsafeMutablePointer<T>.alloc(1) // expected-error {{'alloc' is unavailable: use the 'UnsafeMutablePointer(allocatingCapacity:)' initializer}} {{none}}
  x.dealloc(1) // expected-error {{'dealloc' has been renamed to 'deallocateCapacity'}} {{5-12=deallocateCapacity}} {{none}}
  x.memory = e // expected-error {{'memory' has been renamed to 'pointee'}} {{5-11=pointee}} {{none}}
  x.initialize(e) // expected-error {{'initialize' has been renamed to 'initialize(with:)'}} {{5-15=initialize}} {{16-16=with: }} {{none}}
  x.destroy() // expected-error {{'destroy()' has been renamed to 'deinitialize(count:)'}} {{5-12=deinitialize}} {{none}}
  x.destroy(1) // expected-error {{'destroy' has been renamed to 'deinitialize(count:)'}} {{5-12=deinitialize}} {{13-13=count: }} {{none}}
}

func _VarArgs() {
  func fn1(_: CVarArgType) {} // expected-error {{'CVarArgType' has been renamed to 'CVarArg'}} {{15-26=CVarArg}}{{none}}
  func fn2(_: VaListBuilder) {} // expected-error {{'VaListBuilder' is unavailable}} {{none}}
}

func _Zip<S1 : Sequence, S2: Sequence>(_: S1, _: S2) {
  _ = Zip2Sequence<S1, S2>.Generator.self // expected-error {{'Generator' has been renamed to 'Iterator'}} {{28-37=Iterator}} {{none}}
}
