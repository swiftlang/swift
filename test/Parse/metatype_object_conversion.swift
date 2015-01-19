// RUN: %target-parse-verify-swift

class C {}
struct S {}

protocol NonClassProto {}
protocol ClassConstrainedProto : class {}

func takesAnyObject(x: AnyObject) {}

func concreteTypes() {
  takesAnyObject(C.self) 
  // TODO: Better error messages
  takesAnyObject(S.self) // expected-error{{cannot invoke 'takesAnyObject' with an argument list of type '(S.Type)'}} expected-note{{expected an argument list of type '(AnyObject)'}}
  takesAnyObject(ClassConstrainedProto.self) // expected-error{{cannot invoke 'takesAnyObject' with an argument list of type '(ClassConstrainedProto.Protocol)'}} // expected-note{{expected an argument list of type '(AnyObject)'}}
}

func existentialMetatypes(nonClass: NonClassProto.Type,
                          classConstrained: ClassConstrainedProto.Type,
                          compo: protocol<NonClassProto, ClassConstrainedProto>.Type) {
  takesAnyObject(nonClass) // expected-error{{cannot invoke 'takesAnyObject' with an argument list of type '(NonClassProto.Type)'}} expected-note{{expected an argument list of type '(AnyObject)'}}
  takesAnyObject(classConstrained)
  takesAnyObject(compo)
}
