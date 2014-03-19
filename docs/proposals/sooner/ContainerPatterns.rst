:orphan:

::

  /// Types that conform to this protocol may be pattern-matched by
  /// array patterns in 'switch' cases. This is a refinement of Sequence
  /// because some types may be useful in 'for' loops but don't make sense
  /// to be matched like arrays, in particular, sets and dictionaries.
  protocol ArrayPatternMatchable: Sequence { }


  /// Types that conform to this protocol may be pattern-matched by
  /// dictionary patterns in 'switch' cases.
  protocol DictionaryPatternMatchable {
    typealias KeyType, ValueType

    /// Return the value associated with a key if present, or nil if not
    /// present. Used to match a particular 'key: value' pair in the pattern:
    /// if find(key) returns nil, the pattern match fails; otherwise, it
    /// proceeds by matching the result to the 'value' pattern.
    func find(KeyType) -> ValueType?

    /// Return the number of key-value pairs. Used to test that the size
    /// of the container matches the size of the pattern, unless the pattern
    /// was made size-invariant by '...'.
    var count: Int { get }
  }

  // Array pattern matching works by advancing a generator for the subject.
  switch arr {
  // Matches if:
  //   generator.next() matches .None
  case []:

  // Matches if:
  //   generator.next() matches .Some(let x), then
  //   generator.next() matches .None
  case [let x]:

  // Matches if:
  //   generator.next() matches .Some(let x), then
  //   generator.next() matches .Some(let y), then
  //   generator.next() matches .None
  case [let x, let y]:

  // Matches if:
  //   generator.next() matches .Some(let x), then
  //   generator.next() matches .Some(let y)
  case [let x, let y, ...]:

  // Matches if:
  //   generator.next() matches .Some(let x), then
  //   generator.next() matches .Some(let y), then
  //   generator        matches let rest
  //    -- which always succeeds but binds 'rest' to the generator, which can
  //       subsequently be advanced in the block
  case [let x, let y, let rest...]:

  }
  // TODO: As a refinement, having a specialized protocol for pattern-matching
  // into random access container sequences might be more efficient, since we
  // could potentially load multiple elements instead of loading them one at a
  // time.

  // Dictionary pattern matching works by matching out a series of calls to
  // 'find' and 'count' for the keys of interest. The keys are *not* patterns
  // but are evaluated as expressions.

  // switch (dict.find("foo"), dict.find("bar"), dict.find("bas"), dict.count) {
  switch dict {

  // Dictionary patterns of fixed size:
  // case (.Some(let x), _, _, 1):
  case ["foo": let x]:

  // case (.Some(let x), .Some(let y), _, 2):
  case ["foo": let x, "bar": let y]:

  // case (.Some(let x), _, .Some(let y), 2):
  case ["foo": let x, "bas": let y]:

  // Dictionary patterns of variable size with '...':
  // case (.Some(let x), _, _, _):
  case ["foo": let x, ...]:

  }
  // TODO: How do we unique key expressions across cases? We may need to limit
  // them to literals or simple variable/property references.

  // TODO: Set patterns?
