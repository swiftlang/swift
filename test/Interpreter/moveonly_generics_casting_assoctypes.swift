// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all -enable-experimental-feature SuppressedAssociatedTypes)
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all -enable-experimental-feature SuppressedAssociatedTypes)

// REQUIRES: executable_test
// REQUIRES: swift_feature_SuppressedAssociatedTypes

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

protocol P: ~Copyable {
  associatedtype A: ~Copyable
}

protocol Q: ~Copyable { }

struct X<T: P & ~Copyable> { }

extension X: Q where T: ~Copyable, T.A: Copyable { }

struct NC: ~Copyable { }

struct WithCopyable: P {
  typealias A = Int
}

struct WithNonCopyable: P {
  typealias A = NC
}

func tryCastToQ<T: P>(_: T.Type) -> Q? {
  let x = X<T>()
  return x as? Q
}

precondition(tryCastToQ(_: WithCopyable.self) != nil)
precondition(tryCastToQ(_: WithNonCopyable.self) == nil)

