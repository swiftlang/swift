// {"kind":"typecheck","original":"126322ac","signature":"swift::rewriting::RequirementMachine::areReducedTypeParametersEqual(swift::Type, swift::Type) const","signatureNext":"AssociatedTypeInference::inferTypeWitnessesViaValueWitnesses"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  struct c<d: a> {
    e {
    struct f : Sequence {
    typealias Iterator = Array<d.b>.Iterator
      }
    }
