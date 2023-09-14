// RUN: %target-swift-emit-ir -parse-stdlib %s -enable-experimental-feature Embedded -verify

public enum Never {}

@_silgen_name("abort")
func abort() -> Never

@_semantics("typechecker.type(of:)")
public func type<T, Metatype>(of value: T) -> Metatype { abort() }

public typealias AnyObject = Builtin.AnyObject

precedencegroup AssignmentPrecedence { assignment: true }

public func foo(_ x: AnyObject) {
  _ = type(of: x) // expected-error {{existential can cause metadata allocation or locks}}
  // expected-note@-1 {{called from here}}
}
