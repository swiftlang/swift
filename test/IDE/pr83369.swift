// RUN: %batch-code-completion -module-name main

extension Int {}

// Make sure we can resolve P here.
protocol P<X> where X: main.#^COMPLETE^# {
  associatedtype X
}
// COMPLETE: Decl[Protocol]/CurrModule: P[#P#]; name=P
