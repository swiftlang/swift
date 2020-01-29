// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

class C {}
struct S {}

protocol NonClassProto {}
protocol ClassConstrainedProto : class {}

func takesAnyObject(_ x: AnyObject) {}

func concreteTypes() {
  takesAnyObject(C.self) 
  takesAnyObject(S.self) // expected-error{{argument type 'S.Type' expected to be an instance of a class or class-constrained type}}
  takesAnyObject(ClassConstrainedProto.self) // expected-error{{argument type 'ClassConstrainedProto.Protocol' expected to be an instance of a class or class-constrained type}}
}

func existentialMetatypes(nonClass: NonClassProto.Type,
                          classConstrained: ClassConstrainedProto.Type,
                          compo: (NonClassProto & ClassConstrainedProto).Type) {
  takesAnyObject(nonClass) // expected-error{{argument type 'NonClassProto.Type' expected to be an instance of a class or class-constrained type}}
  takesAnyObject(classConstrained)
  takesAnyObject(compo)
}
