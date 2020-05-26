// RUN: %target-swift-frontend -typecheck -import-objc-header %S/Inputs/restated-protocol-requirement.h %s

// REQUIRES: objc_interop

func giveMeAQ(_ q: Q) {
  _ = q.property
  _ = q.method()
  _ = q[q]
  _ = type(of: q).init(foo: q)
}
