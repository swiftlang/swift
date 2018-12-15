// RUN: %target-typecheck-verify-swift

prefix  operator +++
postfix operator ---

protocol P : class {}

class F<T> {
  func wait() throws -> T { fatalError() }
}

func foo(_ v: AnyObject?) {}
func bar(_ a: AnyObject?, _ b: AnyObject?) {}

prefix  func +++(_ v: AnyObject?) {}
postfix func ---(_ v: AnyObject?) {}

func baz(_ a: F<P>, _ b: F<P>) throws {
  _ = (try  a.wait()) === (try  b.wait())   // Ok
  _ = (try? a.wait()) === (try? b.wait())   // Ok
  _ = foo(try? a.wait())                    // Ok
  _ = foo((try? a.wait()))                  // Ok
  _ = bar((try? a.wait()), (try? b.wait())) // Ok
  _ = +++(try? a.wait())                    // Ok
  _ = +++((try? a.wait()))                  // Ok
  _ = (try? a.wait())---                    // Ok
  _ = ((try? a.wait()))---                  // Ok
}
