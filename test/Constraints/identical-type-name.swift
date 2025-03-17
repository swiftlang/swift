// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(A)) -module-name A -emit-module -emit-module-path %t/A.swiftmodule %S/Inputs/identical-type-name-module.swift
// RUN: %target-build-swift-dylib(%t/%target-library-name(B)) -module-name B -emit-module -emit-module-path %t/B.swiftmodule %S/Inputs/identical-type-name-module.swift
// RUN: %target-typecheck-verify-swift -I %t

import A
import B

func takesConcreteInstance(_ a: A.SomeStruct) {}
func takesGenericInstance<T>(_: T) {}
func takesSameGenericInstance<T>(_: T, _: T) {}
func takesGenericInstanceWithSameTypeRequirement<T: Sequence, U: Sequence>(_: T, _: U) where T.Element == U.Element {}
func takesConstrainedInstance<T: A.SomeProtocol>(_: T) {}
// expected-note@-1 {{where 'T' = 'SomeStruct'}}
// expected-note@-2 {{where 'T' = 'some SomeProtocol'}}
func takesConcreteType(_: A.SomeStruct.Type) {}
func takesGenericType<T>(_: T.Type) {}
func takesSameGenericType<T>(_: T.Type, _: T.Type) {}
func takesGenericTypeWithSameTypeRequirement<T: Sequence, U: Sequence>(_: T.Type, _: U.Type) where T.Element == U.Element {}
// expected-note@-1 {{in call to function 'takesGenericTypeWithSameTypeRequirement'}}
// expected-note@-2 {{where 'T.Element' = 'A.SomeStruct', 'U.Element' = 'B.SomeStruct'}}
func takesConstrainedType<T: A.SomeProtocol>(_: T.Type) {} // expected-note {{where 'T' = 'SomeStruct'}}
func returnsConcreteInstance() -> A.SomeStruct { SomeStruct() }
func returnsGenericInstance<T>() -> T { fatalError() }
func returnsConcreteType() -> A.SomeStruct.Type { SomeStruct.self }
func returnsGenericType<T>() -> T.Type { T.self }

func callit<T>(_ f: () -> T) -> T {
  f()
}

func test() {
  takesConcreteInstance(SomeStruct())
  takesConcreteInstance(A.SomeStruct())
  takesConcreteInstance(B.SomeStruct()) // expected-error {{cannot convert value of type 'B.SomeStruct' to expected argument type 'A.SomeStruct'}}

  takesGenericInstance(SomeStruct()) // expected-error {{ambiguous use of 'init()'}}
  takesGenericInstance(A.SomeStruct())
  takesGenericInstance(B.SomeStruct())

  takesSameGenericInstance(SomeStruct(), SomeStruct()) // expected-error {{ambiguous use of 'init()'}}
  takesSameGenericInstance(A.SomeStruct(), A.SomeStruct())
  takesSameGenericInstance(B.SomeStruct(), B.SomeStruct())
  takesSameGenericInstance(A.SomeStruct(), B.SomeStruct()) // expected-error {{conflicting arguments to generic parameter 'T' ('A.SomeStruct' vs. 'B.SomeStruct')}}

  takesSameGenericInstance(A.returnsOpaqueInstance(), A.returnsOpaqueInstance())
  takesSameGenericInstance(B.returnsOpaqueInstance(), B.returnsOpaqueInstance())
  takesSameGenericInstance(A.returnsOpaqueInstance(), B.returnsOpaqueInstance()) // expected-error {{conflicting arguments to generic parameter 'T' ('some A.SomeProtocol' (result type of 'returnsOpaqueInstance') vs. 'some B.SomeProtocol' (result type of 'returnsOpaqueInstance'))}}

  takesGenericInstanceWithSameTypeRequirement(Array<SomeStruct>(), Array<SomeStruct>())
  // expected-error@-1 {{'SomeStruct' is ambiguous for type lookup in this context}}
  // expected-error@-2 {{'SomeStruct' is ambiguous for type lookup in this context}}
  takesGenericInstanceWithSameTypeRequirement(Array<A.SomeStruct>(), Array<A.SomeStruct>())
  takesGenericInstanceWithSameTypeRequirement(Array<B.SomeStruct>(), Array<B.SomeStruct>())
  takesGenericInstanceWithSameTypeRequirement(Array<A.SomeStruct>(), Array<B.SomeStruct>()) // expected-error {{type of expression is ambiguous without a type annotation}}

  takesConstrainedInstance(SomeStruct())
  takesConstrainedInstance(A.SomeStruct())
  takesConstrainedInstance(B.SomeStruct()) // expected-error {{global function 'takesConstrainedInstance' requires that 'SomeStruct' conform to 'SomeProtocol'}}

  takesConstrainedInstance(A.returnsOpaqueInstance())
  takesConstrainedInstance(B.returnsOpaqueInstance()) // expected-error {{global function 'takesConstrainedInstance' requires that 'some SomeProtocol' conform to 'SomeProtocol'}}

  takesConcreteType(SomeStruct.self)
  takesConcreteType(A.SomeStruct.self)
  takesConcreteType(B.SomeStruct.self) // expected-error {{cannot convert value of type 'B.SomeStruct.Type' to expected argument type 'A.SomeStruct.Type'}}

  takesGenericType(SomeStruct.self) // expected-error {{conflicting arguments to generic parameter 'T' ('A.SomeStruct' vs. 'B.SomeStruct')}}
  takesGenericType(A.SomeStruct.self)
  takesGenericType(B.SomeStruct.self)

  takesSameGenericType(SomeStruct.self, SomeStruct.self) // expected-error {{conflicting arguments to generic parameter 'T' ('A.SomeStruct' vs. 'B.SomeStruct')}}
  takesSameGenericType(A.SomeStruct.self, A.SomeStruct.self)
  takesSameGenericType(B.SomeStruct.self, B.SomeStruct.self)
  takesSameGenericType(A.SomeStruct.self, B.SomeStruct.self) // expected-error {{cannot convert value of type 'B.SomeStruct.Type' to expected argument type 'A.SomeStruct.Type'}}

  takesGenericTypeWithSameTypeRequirement(Array<SomeStruct>.self, Array<SomeStruct>.self)
  // expected-error@-1 {{'SomeStruct' is ambiguous for type lookup in this context}}
  // expected-error@-2 {{'SomeStruct' is ambiguous for type lookup in this context}}
  // expected-error@-3 {{generic parameter 'T' could not be inferred}}
  // expected-error@-4 {{generic parameter 'U' could not be inferred}}
  takesGenericTypeWithSameTypeRequirement(Array<A.SomeStruct>.self, Array<A.SomeStruct>.self)
  takesGenericTypeWithSameTypeRequirement(Array<B.SomeStruct>.self, Array<B.SomeStruct>.self)
  takesGenericTypeWithSameTypeRequirement(Array<A.SomeStruct>.self, Array<B.SomeStruct>.self)
  // expected-error@-1 {{global function 'takesGenericTypeWithSameTypeRequirement' requires the types 'A.SomeStruct' and 'B.SomeStruct' be equivalent}}

  takesConstrainedType(SomeStruct.self)
  takesConstrainedType(A.SomeStruct.self)
  takesConstrainedType(B.SomeStruct.self) // expected-error {{global function 'takesConstrainedType' requires that 'SomeStruct' conform to 'SomeProtocol'}}

  let _: SomeStruct = returnsConcreteInstance() // expected-error {{'SomeStruct' is ambiguous for type lookup in this context}}
  let _: A.SomeStruct = returnsConcreteInstance()
  let _: B.SomeStruct = returnsConcreteInstance() // expected-error {{cannot convert value of type 'A.SomeStruct' to specified type 'B.SomeStruct'}}

  let _: SomeStruct = returnsGenericInstance() // expected-error {{'SomeStruct' is ambiguous for type lookup in this context}}
  let _: A.SomeStruct = returnsGenericInstance()
  let _: B.SomeStruct = returnsGenericInstance()

  let _: SomeStruct.Type = returnsConcreteType() // expected-error {{'SomeStruct' is ambiguous for type lookup in this context}}
  let _: A.SomeStruct.Type = returnsConcreteType()
  let _: B.SomeStruct.Type = returnsConcreteType() // expected-error {{cannot convert value of type 'A.SomeStruct.Type' to specified type 'B.SomeStruct.Type'}}

  let _: SomeStruct.Type = returnsGenericType() // expected-error {{'SomeStruct' is ambiguous for type lookup in this context}}
  let _: A.SomeStruct.Type = returnsGenericType()
  let _: B.SomeStruct.Type = returnsGenericType()

  let _ = callit { SomeStruct() } // expected-error {{ambiguous use of 'init()'}}
  let _ = callit { A.SomeStruct() }
  let _: B.SomeStruct = callit { A.SomeStruct() } // expected-error {{conflicting arguments to generic parameter 'T' ('A.SomeStruct' vs. 'B.SomeStruct')}}
  // expected-note@-1 {{generic parameter 'T' inferred as 'SomeStruct' from context}}
  // expected-note@-2 {{generic parameter 'T' inferred as 'SomeStruct' from closure return expression}}
  let _: A.SomeStruct = callit { A.SomeStruct() }
  let _: B.SomeStruct = callit { B.SomeStruct() }
}

struct Associate {}
func takesTopLevelAssociate(_ a: Associate) {}

func takesGenericAssociate<Associate>(_ a: Associate) {
  takesTopLevelAssociate(a) // expected-error {{cannot convert value of type 'Associate' to expected argument type 'main.Associate'}}
}

protocol P {
  associatedtype Associate
}

extension P {
  func f(_ a: Associate) {
    takesTopLevelAssociate(a) // expected-error {{cannot convert value of type 'Self.Associate' to expected argument type 'Associate'}}
  }
}
