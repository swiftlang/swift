// RUN: %target-run-simple-swift(-I %S/Inputs/concrete-template-types -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxConcreteTemplateTypes)
// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxConcreteTemplateTypes

import ConcreteTemplateTypes

func pointer(_ value: concrete.Result<UnsafeMutableRawPointer>) -> concrete.Result<UnsafeMutableRawPointer> { value }
func empty(_ value: concrete.Result<Void>) -> concrete.Result<Void> { value }
func integer(_ value: concrete.Result<CInt>) -> concrete.IntResult { value }
func immutable(_ value: concrete.Result<UnsafeRawPointer>) -> concrete.Result<UnsafeRawPointer> { value }

precondition(pointer(concrete.makePointer()).value == nil)
precondition(empty(concrete.makeVoid()).value == 42)
precondition(integer(concrete.makeInt()).value == 23)
precondition(immutable(concrete.makeConstPointer()).value == nil)
precondition(ObjectIdentifier(concrete.Result<CInt>.self) == ObjectIdentifier(concrete.IntResult.self))
