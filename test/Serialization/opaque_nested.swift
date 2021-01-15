// RUN: %target-build-swift -Xfrontend -disable-availability-checking -emit-module -module-name A -Xfrontend -experimental-skip-all-function-bodies -Xfrontend -debug-forbid-typecheck-prefix -Xfrontend NEVERTYPECHECK %s
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -emit-module -module-name A -Xfrontend -experimental-skip-non-inlinable-function-bodies -Xfrontend -debug-forbid-typecheck-prefix -Xfrontend NEVERTYPECHECK %s

protocol Base {
  func anything()
}

func test() {
  struct Nested : Base {
    let NEVERTYPECHECK_property = 1

    func anything() {
      let NEVERTYPECHECK_local = 1
    }

    func opaqueReturnType() -> some Base {
      let NEVERTYPECHECK_local = 1
      return Nested()
    }
  }
}
