// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

class C {}
struct S {}

protocol NonClassProto {}
protocol ClassConstrainedProto : class {}

func takesAnyObject(_ x: AnyObject) {}

func concreteTypes() {
  takesAnyObject(C.self) 
  // TODO: Better error messages
  takesAnyObject(S.self) // expected-error{{argument type 'S.Type' does not conform to expected type 'AnyObject'}}
  takesAnyObject(ClassConstrainedProto.self) // expected-error{{argument type 'ClassConstrainedProto.Protocol' does not conform to expected type 'AnyObject'}}
}

func existentialMetatypes(nonClass: NonClassProto.Type,
                          classConstrained: ClassConstrainedProto.Type,
                          compo: (NonClassProto & ClassConstrainedProto).Type) {
  takesAnyObject(nonClass) // expected-error{{argument type 'NonClassProto.Type' does not conform to expected type 'AnyObject'}}
  takesAnyObject(classConstrained)
  takesAnyObject(compo)
}
