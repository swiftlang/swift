// RUN: %target-swift-frontend -enable-experimental-opened-existential-types -typecheck -dump-ast -parse-as-library %s | %FileCheck %s

protocol P { }
extension Optional: P where Wrapped: P { }

func acceptsBox<T>(_ value: T) { }

// CHECK: passBox
// CHECK-NOT: open_existential_expr
func passBox(p: P, obj: AnyObject, err: Error) {
  acceptsBox(p as P)
  acceptsBox(p as! P)
  acceptsBox(p as? P)
  acceptsBox(obj)
  acceptsBox(err)
}
