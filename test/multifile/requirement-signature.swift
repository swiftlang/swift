// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/requirement-signature.swift

func run<CO: ConnectableObservableType, O>(co: CO, o: O) {
  co.subscribe(o)
}
