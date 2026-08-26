// RUN: %target-swift-frontend -I %S/Inputs/TypeOfActionBase -I %S/Inputs/TypeOfActionDerived %s -emit-sil -o /dev/null -verify -swift-version 6

// REQUIRES: concurrency
// REQUIRES: objc_interop

// `type(of:)` borrows its operand instead of copying it. When the operand is a
// loadable value held in storage, that borrow has to be loaded before it is
// handed to `value_metatype`; passing the address of the storage instead used to
// trip "Can only accept non-Sendable values" in RegionAnalysis, which classifies
// the address of a capture box differently than the value inside it.
//
// Reproducing that takes several things at once: the operand has to be read out
// of a capture box (so an escaping closure -- here an escaping autoclosure), it
// has to be a `sending` parameter whose region is then sent onward, and the base
// class has to come from a `@preconcurrency` import while the class it is cast
// to comes from an ordinary one. That split is what makes the two Sendable
// classifications disagree.

@preconcurrency import TypeOfActionBase
import TypeOfActionDerived

enum ActionError: Error { case unsupported }

func log(_ message: @autoclosure @escaping () -> Any.Type) {}

struct Handler {
  func handle(from action: sending BaseAction) throws {
    log(type(of: action))
    switch action {
    case let action as DerivedAction:
      return try handleDerived(action)
    default:
      throw ActionError.unsupported
    }
  }

  func handleDerived(_ action: DerivedAction) throws { fatalError() }
}

// An address-only operand is legitimately read in place, so it has to keep
// working through the same path.
protocol ActionProtocol {}
struct OpaqueAction: ActionProtocol { var action = BaseAction() }

func addressOnlyOperand(from action: sending any ActionProtocol) {
  log(type(of: action))
}

func genericOperand<T>(from action: sending T) {
  log(type(of: action))
}

// The same shape in an async context, where region analysis tracks more.
func asyncOperand(from action: sending BaseAction) async {
  log(type(of: action))
}
