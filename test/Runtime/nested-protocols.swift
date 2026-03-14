// RUN: %target-run-simple-swift

// REQUIRES: executable_test

enum E {
  protocol P {}
}

struct Conforms {}
extension Conforms: E.P {}

struct DoesNotConform {}

struct S<T> {}
extension S: E.P where T: E.P {}

@inline(never)
func castToNested(_ value: Any) -> (any E.P)? {
    value as? any E.P
}

// Regular conformance.

precondition(castToNested(Conforms()) != nil)
precondition(castToNested(DoesNotConform()) == nil)

// Conditional conformance.

precondition(castToNested(S<Conforms>()) != nil)
precondition(castToNested(S<DoesNotConform>()) == nil)

// Verify that 'E.P' and a non-nested 'P' are different.

protocol P {}

@inline(never)
func castToNonNested(_ value: Any) -> (any P)? {
    value as? any P
}

precondition(castToNonNested(Conforms()) == nil)
precondition(castToNonNested(S<Conforms>()) == nil)
precondition(castToNonNested(DoesNotConform()) == nil)
precondition(castToNonNested(S<DoesNotConform>()) == nil)

// Local protocols.

@inline(never)
func f0(_ input: Any) -> (Any, inputConforms: Bool) {

  protocol LocalProto { }
  struct ConformsToLocal: LocalProto {}

  if let input = input as? any LocalProto {
    return (input, true)
  } else {
    return (ConformsToLocal(), false)
  }
}

var result = f0(DoesNotConform())
precondition(result.inputConforms == false)
result = f0(result.0)
precondition(result.inputConforms == true)
