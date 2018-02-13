// RUN: %swift -swift-version 4 -typecheck -verify  %s

func flatMapOnSequence<
  S : Sequence
>(xs: S, f: (S.Element) -> S.Element?) {
  _ = xs.flatMap(f) // expected-warning {{'flatMap' is deprecated: Please use compactMap(_:) for the case where closure returns an optional value}} expected-note {{compactMap}}
}

func flatMapOnLazySequence<
  S : LazySequenceProtocol
>(xs: S, f: (S.Element) -> S.Element?) {
  _ = xs.flatMap(f) // expected-warning {{'flatMap' is deprecated: Please use compactMap(_:) for the case where closure returns an optional value}} expected-note {{compactMap}}
}

func flatMapOnLazyCollection<
  C : LazyCollectionProtocol
>(xs: C, f: (C.Element) -> C.Element?) {
  _ = xs.flatMap(f) // expected-warning {{'flatMap' is deprecated: Please use compactMap(_:) for the case where closure returns an optional value}} expected-note {{compactMap}}
}

func flatMapOnLazyBidirectionalCollection<
  C : LazyCollectionProtocol & BidirectionalCollection
>(xs: C, f: (C.Element) -> C.Element?)
where C.Elements : BidirectionalCollection {
  _ = xs.flatMap(f) // expected-warning {{'flatMap' is deprecated: Please use compactMap(_:) for the case where closure returns an optional value}} expected-note {{compactMap}}
}

func flatMapOnCollectinoOfStrings<
  C : Collection
>(xs: C, f: (C.Element) -> String?) {
  _ = xs.flatMap(f) // expected-warning {{'flatMap' is deprecated: Please use compactMap(_:) for the case where closure returns an optional value}} expected-note {{compactMap}}
}
