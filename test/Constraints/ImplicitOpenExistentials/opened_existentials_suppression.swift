// RUN: %target-swift-frontend -typecheck -dump-ast -parse-as-library %s | %FileCheck %s

protocol P { }
extension Optional: P where Wrapped: P { }

func acceptsBox<T>(_ value: T) { }
func acceptsBoxType<T>(_ value: T.Type) { }

// CHECK: passBox
// CHECK-NOT: open_existential_expr
func passBox(p: P, obj: AnyObject, err: Error) {
  acceptsBox(p)
  acceptsBox(p as P)
  acceptsBox(p as! P)
  acceptsBox(p as? P)
  acceptsBox(obj)
  acceptsBox(err)
  acceptsBoxType(Any.self)
  acceptsBoxType(AnyObject.self)
}
