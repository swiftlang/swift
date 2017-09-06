// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules -Xcc -DNO_EXTRA_PROTOCOLS %s
// RUN: %target-swift-frontend -typecheck -DTEST -Xcc -DNO_EXTRA_PROTOCOLS -I %t -I %S/Inputs/custom-modules %s
// RUN: %target-swift-frontend -emit-ir -DTEST -Xcc -DEXTRA_PROTOCOL_FIRST -I %t -I %S/Inputs/custom-modules %s -o /dev/null

// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules -Xcc -DEXTRA_PROTOCOL_FIRST %s
// RUN: %target-swift-frontend -typecheck -DTEST -Xcc -DEXTRA_PROTOCOL_FIRST -I %t -I %S/Inputs/custom-modules %s
// RUN: %target-swift-frontend -emit-ir -DTEST -Xcc -DNO_EXTRA_PROTOCOLS -I %t -I %S/Inputs/custom-modules %s -o /dev/null


// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules -Xcc -DNO_EXTRA_PROTOCOLS %s
// RUN: %target-swift-frontend -typecheck -DTEST -Xcc -DNO_EXTRA_PROTOCOLS -I %t -I %S/Inputs/custom-modules %s
// RUN: %target-swift-frontend -emit-ir -DTEST -Xcc -DEXTRA_PROTOCOL_MIDDLE -I %t -I %S/Inputs/custom-modules %s -o /dev/null

// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules -Xcc -DEXTRA_PROTOCOL_MIDDLE %s
// RUN: %target-swift-frontend -typecheck -DTEST -Xcc -DEXTRA_PROTOCOL_MIDDLE -I %t -I %S/Inputs/custom-modules %s
// RUN: %target-swift-frontend -emit-ir -DTEST -Xcc -DNO_EXTRA_PROTOCOLS -I %t -I %S/Inputs/custom-modules %s -o /dev/null


// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules -Xcc -DNO_EXTRA_PROTOCOLS %s
// RUN: %target-swift-frontend -typecheck -DTEST -Xcc -DNO_EXTRA_PROTOCOLS -I %t -I %S/Inputs/custom-modules %s
// RUN: %target-swift-frontend -emit-ir -DTEST -Xcc -DEXTRA_PROTOCOL_LAST -I %t -I %S/Inputs/custom-modules %s -o /dev/null

// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules -Xcc -DEXTRA_PROTOCOL_LAST %s
// RUN: %target-swift-frontend -typecheck -DTEST -Xcc -DEXTRA_PROTOCOL_LAST -I %t -I %S/Inputs/custom-modules %s
// RUN: %target-swift-frontend -emit-ir -DTEST -Xcc -DNO_EXTRA_PROTOCOLS -I %t -I %S/Inputs/custom-modules %s -o /dev/null


// REQUIRES: objc_interop

#if TEST

import Lib
import ProtocolInheritance

func useSubProto<T: SubProto>(_: T) {}
func useConsistentProto<T: Order2_ConsistentBaseProto>(_: T) {}
func useFickleProto<T: Order1_FickleBaseProto>(_: T) {}

func test(obj: Impl) {
  useConsistentProto(obj)
  useFickleProto(obj)
  useSubProto(obj)
}

protocol ForceDeserializationProto: SubProto {}
extension Impl: ForceDeserializationProto {}

func test(obj: PartialImpl) {
  useConsistentProto(obj)
  useSubProto(obj)
}

extension PartialImpl: ForceDeserializationProto {}

#else // TEST

import ProtocolInheritance

open class Impl: SubProto {
  public func consistent() {}
}

extension Impl: Order1_FickleBaseProto, Order3_FickleBaseProto, Order5_FickleBaseProto {
  public func fickle() {}
}

open class PartialImpl: SubProto {
  public func consistent() {}
  public func fickle() {}
}


#endif // TEST
