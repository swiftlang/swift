// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

class C {}
struct S {}

protocol NonClassProto {}
protocol ClassConstrainedProto : class {}

func takesAnyObject(x: AnyObject) {}

func concreteTypes() {
  takesAnyObject(C.self) 
  // TODO: Better error messages
  takesAnyObject(S.self) // expected-error{{type 'S.Type' does not conform to protocol 'AnyObject'}}
  takesAnyObject(ClassConstrainedProto.self) // expected-error{{type 'ClassConstrainedProto.Protocol' does not conform to protocol 'AnyObject'}}
}

func existentialMetatypes(nonClass: NonClassProto.Type,
                          classConstrained: ClassConstrainedProto.Type,
                          compo: protocol<NonClassProto, ClassConstrainedProto>.Type) {
  takesAnyObject(nonClass) // expected-error{{type 'NonClassProto.Type' does not conform to protocol 'AnyObject'}}
  takesAnyObject(classConstrained)
  takesAnyObject(compo)
}
