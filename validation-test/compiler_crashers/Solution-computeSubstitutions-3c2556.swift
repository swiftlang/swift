// {"kind":"typecheck","original":"622a4b1a","signature":"swift::ProtocolConformanceRef llvm::function_ref<swift::ProtocolConformanceRef (swift::InFlightSubstitution&, swift::Type, swift::ProtocolDecl*)>::callback_fn<swift::constraints::Solution::computeSubstitutions(swift::NullablePtr<swift::ValueDecl>, swift::GenericSignature, swift::constraints::ConstraintLocator*) const::$_0>(long, swift::InFlightSubstitution&, swift::Type, swift::ProtocolDecl*)","signatureNext":"InFlightSubstitution::lookupConformance"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed
struct a: DistributedTargetInvocationEncoder {
  typealias SerializationRequirement = Codable
  mutating func recordReturnType<b: Codable>(b ) throws
