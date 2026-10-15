// RUN: %target-typecheck-verify-swift

protocol P {
  static var x: Int { get }
}
func foo(p: some P) -> Int {
  p.x // expected-error {{static member 'x' cannot be used on instance of type 'some P'}} {{3-3=type(of: }} {{4-4=)}}
}
func bar(p: any P) -> Int {
  p.x // expected-error {{static member 'x' cannot be used on instance of type 'any P'}} {{3-3=type(of: }} {{4-4=)}}
}
