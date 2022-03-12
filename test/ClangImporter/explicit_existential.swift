// RUN: %target-typecheck-verify-swift %clang-importer-sdk -enable-objc-interop -import-objc-header %S/Inputs/explicit_existential.h

// Make sure that 'typedef id<P, Q> PAndQ' imports as a typealias without
// the ExistentialType wrapping the underlying type.

protocol InheritsFromQAndQ : PAndQ {}

func genericOverPAndQ<T : PAndQ>(_: T) {}

func takesSequenceOfPAndQ<T : Sequence>(_: T) where T.Element : PAndQ {}

func takesPAndQExistential(_ x: PAndQ) {
  let b = PAndQProcessor()
  b.takesPAndQExistential(x)
}