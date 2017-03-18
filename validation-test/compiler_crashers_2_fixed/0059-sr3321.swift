// RUN: %target-swift-frontend %s -emit-ir
// RUN: %target-swift-frontend %s -emit-ir -O

// XFAIL: *

// Note: this was passing for the wrong reasons before. We need to
// correctly model cases where a nested type of an abstract conformance is
// in fact concrete and, therefore, has concrete conformances.

protocol ControllerB {
    associatedtype T: Controller
}

protocol Controller {
    associatedtype T

    func shouldSelect<S: ControllerB>(_ a: T, b: S) where S.T == Self
}

struct ControllerAImpl {}

struct ControllerImpl : Controller {
  typealias T = ControllerAImpl

  func shouldSelect<S : ControllerB>(_ a: ControllerAImpl, b: S) where S.T == ControllerImpl {}
}

struct ControllerBImpl1 : ControllerB {
  typealias T = ControllerImpl
}

struct ControllerBImpl2<C : Controller> : ControllerB {
  typealias T = C
}

extension Controller {
    func didSelect<S: ControllerB>(_ a: T, b: S) where S.T == Self {
        shouldSelect(a, b: b)
    }
}

ControllerImpl().didSelect(ControllerAImpl(), b: ControllerBImpl1())
ControllerImpl().didSelect(ControllerAImpl(), b: ControllerBImpl2<ControllerImpl>())
