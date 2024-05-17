// RUN: %target-swift-emit-silgen -module-name sending -disable-availability-checking -enable-upcoming-feature RegionBasedIsolation -enable-experimental-feature SendingArgsAndResults -strict-concurrency=complete %s | %FileCheck %s

class NonSendable {}

// CHECK-LABEL: sil hidden [ossa] @$s7sending15returnsSendableSSyYTF : $@convention(thin) () -> @sil_sending @owned String {
func returnsSendable() -> sending String { fatalError() }
// CHECK-LABEL: sil hidden [ossa] @$s7sending18returnsNonSendableAA0cD0CyYTF : $@convention(thin) () -> @sil_sending @owned NonSendable {
func returnsNonSendable() -> sending NonSendable { fatalError() }
// CHECK-LABEL: sil hidden [ossa] @$s7sending25genericReturnsNonSendablexyYTlF : $@convention(thin) <T> () -> @sil_sending @out T {
func genericReturnsNonSendable<T>() -> sending T { fatalError() }

actor MyActor {
  // CHECK-LABEL: sil hidden [ossa] @$s7sending7MyActorC15returnsSendableSSyYTF : $@convention(method) (@sil_isolated @guaranteed MyActor) -> @sil_sending @owned String {
  func returnsSendable() -> sending String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending7MyActorC18returnsNonSendableAA0eF0CyYTF : $@convention(method) (@sil_isolated @guaranteed MyActor) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> sending NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending7MyActorC25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (@sil_isolated @guaranteed MyActor) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> sending T { fatalError() }
}

struct Struct {
  // CHECK-LABEL: sil hidden [ossa] @$s7sending6StructV15returnsSendableSSyYTF : $@convention(method) (Struct) -> @sil_sending @owned String {
  func returnsSendable() -> sending String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending6StructV18returnsNonSendableAA0dE0CyYTF : $@convention(method) (Struct) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> sending NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending6StructV25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (Struct) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> sending T { fatalError() }
}

@MainActor
struct MainActorStruct {
  // CHECK-LABEL: sil hidden [ossa] @$s7sending15MainActorStructV15returnsSendableSSyYTF : $@convention(method) (MainActorStruct) -> @sil_sending @owned String {
  func returnsSendable() -> sending String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending15MainActorStructV18returnsNonSendableAA0fG0CyYTF : $@convention(method) (MainActorStruct) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> sending NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending15MainActorStructV25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (MainActorStruct) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> sending T { fatalError() }
}

class Class {
  // CHECK-LABEL: sil hidden [ossa] @$s7sending5ClassC15returnsSendableSSyYTF : $@convention(method) (@guaranteed Class) -> @sil_sending @owned String {
  func returnsSendable() -> sending String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending5ClassC18returnsNonSendableAA0dE0CyYTF : $@convention(method) (@guaranteed Class) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> sending NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending5ClassC25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (@guaranteed Class) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> sending T { fatalError() }
}

final class FinalClass {
  // CHECK-LABEL: sil hidden [ossa] @$s7sending10FinalClassC15returnsSendableSSyYTF : $@convention(method) (@guaranteed FinalClass) -> @sil_sending @owned String {
  func returnsSendable() -> sending String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending10FinalClassC18returnsNonSendableAA0eF0CyYTF : $@convention(method) (@guaranteed FinalClass) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> sending NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending10FinalClassC25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (@guaranteed FinalClass) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> sending T { fatalError() }
}

@MainActor
class MainActorClass {
  // CHECK-LABEL: sil hidden [ossa] @$s7sending14MainActorClassC15returnsSendableSSyYTF : $@convention(method) (@guaranteed MainActorClass) -> @sil_sending @owned String {
  func returnsSendable() -> sending String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending14MainActorClassC18returnsNonSendableAA0fG0CyYTF : $@convention(method) (@guaranteed MainActorClass) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> sending NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending14MainActorClassC25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (@guaranteed MainActorClass) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> sending T { fatalError() }
}

@MainActor
final class FinalMainActorClass {
  // CHECK-LABEL: sil hidden [ossa] @$s7sending19FinalMainActorClassC15returnsSendableSSyYTF : $@convention(method) (@guaranteed FinalMainActorClass) -> @sil_sending @owned String {
  func returnsSendable() -> sending String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending19FinalMainActorClassC18returnsNonSendableAA0gH0CyYTF : $@convention(method) (@guaranteed FinalMainActorClass) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> sending NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending19FinalMainActorClassC25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (@guaranteed FinalMainActorClass) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> sending T { fatalError() }
}

enum Enum {
  case myCase
  // CHECK-LABEL: sil hidden [ossa] @$s7sending4EnumO15returnsSendableSSyYTF : $@convention(method) (Enum) -> @sil_sending @owned String {
  func returnsSendable() -> sending String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending4EnumO18returnsNonSendableAA0dE0CyYTF : $@convention(method) (Enum) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> sending NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending4EnumO25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (Enum) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> sending T { fatalError() }
}

@MainActor
enum MainActorEnum {
  case myCase
  // CHECK-LABEL: sil hidden [ossa] @$s7sending13MainActorEnumO15returnsSendableSSyYTF : $@convention(method) (MainActorEnum) -> @sil_sending @owned String {
  func returnsSendable() -> sending String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending13MainActorEnumO18returnsNonSendableAA0fG0CyYTF : $@convention(method) (MainActorEnum) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> sending NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending13MainActorEnumO25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (MainActorEnum) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> sending T { fatalError() }
}

protocol P {
  func protReturnsSendable() -> sending String
  func protReturnsNonSendable() -> sending NonSendable
  func protGenericReturnsNonSendable<T>() -> sending T
}

extension P {
  // CHECK-LABEL: sil hidden [ossa] @$s7sending1PPAAE19protReturnsSendableSSyYTF : $@convention(method) <Self where Self : P> (@in_guaranteed Self) -> @sil_sending @owned String {
  func protReturnsSendable() -> sending String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending1PPAAE22protReturnsNonSendableAA0dE0CyYTF : $@convention(method) <Self where Self : P> (@in_guaranteed Self) -> @sil_sending @owned NonSendable {
  func protReturnsNonSendable() -> sending NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending1PPAAE29protGenericReturnsNonSendableqd__yYTlF : $@convention(method) <Self where Self : P><T> (@in_guaranteed Self) -> @sil_sending @out T {
  func protGenericReturnsNonSendable<T>() -> sending T { fatalError() }
}

extension Struct : P {
  // CHECK-LABEL: sil hidden [ossa] @$s7sending6StructV19protReturnsSendableSSyYTF : $@convention(method) (Struct) -> @sil_sending @owned String {
  func protReturnsSendable() -> sending String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending6StructV22protReturnsNonSendableAA0eF0CyYTF : $@convention(method) (Struct) -> @sil_sending @owned NonSendable {
  func protReturnsNonSendable() -> sending NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s7sending6StructV29protGenericReturnsNonSendablexyYTlF : $@convention(method) <T> (Struct) -> @sil_sending @out T {
  func protGenericReturnsNonSendable<T>() -> sending T { fatalError() }
}
