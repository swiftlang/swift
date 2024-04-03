// RUN: %target-typecheck-verify-swift

protocol PerturberProtocol {
  associatedtype CP: Perturbation
}

protocol Perturbation {
  associatedtype Perturber: PerturberProtocol where Self == Perturber.CP
}

protocol IDCMemberFunctionAddition: Perturbation {
  // This type alias should not cause a crash.
  typealias Perturber = MemberAdder
}

class MemberAdder<CP: IDCMemberFunctionAddition>: PerturberProtocol {
}
