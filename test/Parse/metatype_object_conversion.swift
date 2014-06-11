// RUN: %swift -enable-metatype-object-conversions -parse -verify %s

class C {}
struct S {}

protocol NonClassProto {}
@class_protocol protocol ClassConstrainedProto {}

func takesAnyObject(x: AnyObject) {} // expected-note{{}} // expected-note{{}} // expected-note{{}}

func concreteTypes() {
  takesAnyObject(C.self) 
  // TODO: Better error messages
  takesAnyObject(S.self) // expected-error{{'S.Type' does not conform to protocol 'AnyObject'}}
  takesAnyObject(ClassConstrainedProto.self) // expected-error{{'ClassConstrainedProto.Protocol' does not conform to protocol 'AnyObject'}}
}

func existentialMetatypes(nonClass: NonClassProto.Type,
                          classConstrained: ClassConstrainedProto.Type,
                          compo: protocol<NonClassProto, ClassConstrainedProto>.Type) {
  takesAnyObject(nonClass) // expected-error{{'NonClassProto.Type' does not conform to protocol 'AnyObject'}}
  takesAnyObject(classConstrained)
  takesAnyObject(compo)
}
