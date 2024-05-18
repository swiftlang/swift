// RUN: %target-swift-emit-silgen -disable-availability-checking -enable-upcoming-feature RegionBasedIsolation -enable-experimental-feature TransferringArgsAndResults -strict-concurrency=complete %s | %FileCheck %s

class NonSendable {}

// CHECK-LABEL: sil hidden [ossa] @$s12transferring15returnsSendableSSyYTF : $@convention(thin) () -> @sil_sending @owned String {
func returnsSendable() -> transferring String { fatalError() }
// CHECK-LABEL: sil hidden [ossa] @$s12transferring18returnsNonSendableAA0cD0CyYTF : $@convention(thin) () -> @sil_sending @owned NonSendable {
func returnsNonSendable() -> transferring NonSendable { fatalError() }
// CHECK-LABEL: sil hidden [ossa] @$s12transferring25genericReturnsNonSendablexyYTlF : $@convention(thin) <T> () -> @sil_sending @out T {
func genericReturnsNonSendable<T>() -> transferring T { fatalError() }

actor MyActor {
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring7MyActorC15returnsSendableSSyYTF : $@convention(method) (@sil_isolated @guaranteed MyActor) -> @sil_sending @owned String {
  func returnsSendable() -> transferring String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring7MyActorC18returnsNonSendableAA0eF0CyYTF : $@convention(method) (@sil_isolated @guaranteed MyActor) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> transferring NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring7MyActorC25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (@sil_isolated @guaranteed MyActor) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> transferring T { fatalError() }
}

struct Struct {
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring6StructV15returnsSendableSSyYTF : $@convention(method) (Struct) -> @sil_sending @owned String {
  func returnsSendable() -> transferring String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring6StructV18returnsNonSendableAA0dE0CyYTF : $@convention(method) (Struct) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> transferring NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring6StructV25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (Struct) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> transferring T { fatalError() }
}

@MainActor
struct MainActorStruct {
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring15MainActorStructV15returnsSendableSSyYTF : $@convention(method) (MainActorStruct) -> @sil_sending @owned String {
  func returnsSendable() -> transferring String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring15MainActorStructV18returnsNonSendableAA0fG0CyYTF : $@convention(method) (MainActorStruct) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> transferring NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring15MainActorStructV25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (MainActorStruct) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> transferring T { fatalError() }
}

class Class {
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring5ClassC15returnsSendableSSyYTF : $@convention(method) (@guaranteed Class) -> @sil_sending @owned String {
  func returnsSendable() -> transferring String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring5ClassC18returnsNonSendableAA0dE0CyYTF : $@convention(method) (@guaranteed Class) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> transferring NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring5ClassC25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (@guaranteed Class) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> transferring T { fatalError() }
}

final class FinalClass {
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring10FinalClassC15returnsSendableSSyYTF : $@convention(method) (@guaranteed FinalClass) -> @sil_sending @owned String {
  func returnsSendable() -> transferring String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring10FinalClassC18returnsNonSendableAA0eF0CyYTF : $@convention(method) (@guaranteed FinalClass) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> transferring NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring10FinalClassC25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (@guaranteed FinalClass) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> transferring T { fatalError() }
}

@MainActor
class MainActorClass {
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring14MainActorClassC15returnsSendableSSyYTF : $@convention(method) (@guaranteed MainActorClass) -> @sil_sending @owned String {
  func returnsSendable() -> transferring String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring14MainActorClassC18returnsNonSendableAA0fG0CyYTF : $@convention(method) (@guaranteed MainActorClass) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> transferring NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring14MainActorClassC25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (@guaranteed MainActorClass) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> transferring T { fatalError() }
}

@MainActor
final class FinalMainActorClass {
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring19FinalMainActorClassC15returnsSendableSSyYTF : $@convention(method) (@guaranteed FinalMainActorClass) -> @sil_sending @owned String {
  func returnsSendable() -> transferring String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring19FinalMainActorClassC18returnsNonSendableAA0gH0CyYTF : $@convention(method) (@guaranteed FinalMainActorClass) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> transferring NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring19FinalMainActorClassC25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (@guaranteed FinalMainActorClass) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> transferring T { fatalError() }
}

enum Enum {
  case myCase
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring4EnumO15returnsSendableSSyYTF : $@convention(method) (Enum) -> @sil_sending @owned String {
  func returnsSendable() -> transferring String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring4EnumO18returnsNonSendableAA0dE0CyYTF : $@convention(method) (Enum) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> transferring NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring4EnumO25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (Enum) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> transferring T { fatalError() }
}

@MainActor
enum MainActorEnum {
  case myCase
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring13MainActorEnumO15returnsSendableSSyYTF : $@convention(method) (MainActorEnum) -> @sil_sending @owned String {
  func returnsSendable() -> transferring String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring13MainActorEnumO18returnsNonSendableAA0fG0CyYTF : $@convention(method) (MainActorEnum) -> @sil_sending @owned NonSendable {
  func returnsNonSendable() -> transferring NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring13MainActorEnumO25genericReturnsNonSendablexyYTlF : $@convention(method) <T> (MainActorEnum) -> @sil_sending @out T {
  func genericReturnsNonSendable<T>() -> transferring T { fatalError() }
}

protocol P {
  func protReturnsSendable() -> transferring String
  func protReturnsNonSendable() -> transferring NonSendable
  func protGenericReturnsNonSendable<T>() -> transferring T
}

extension P {
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring1PPAAE19protReturnsSendableSSyYTF : $@convention(method) <Self where Self : P> (@in_guaranteed Self) -> @sil_sending @owned String {
  func protReturnsSendable() -> transferring String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring1PPAAE22protReturnsNonSendableAA0dE0CyYTF : $@convention(method) <Self where Self : P> (@in_guaranteed Self) -> @sil_sending @owned NonSendable {
  func protReturnsNonSendable() -> transferring NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring1PPAAE29protGenericReturnsNonSendableqd__yYTlF : $@convention(method) <Self where Self : P><T> (@in_guaranteed Self) -> @sil_sending @out T {
  func protGenericReturnsNonSendable<T>() -> transferring T { fatalError() }
}

extension Struct : P {
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring6StructV19protReturnsSendableSSyYTF : $@convention(method) (Struct) -> @sil_sending @owned String {
  func protReturnsSendable() -> transferring String { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring6StructV22protReturnsNonSendableAA0eF0CyYTF : $@convention(method) (Struct) -> @sil_sending @owned NonSendable {
  func protReturnsNonSendable() -> transferring NonSendable { fatalError() }
  // CHECK-LABEL: sil hidden [ossa] @$s12transferring6StructV29protGenericReturnsNonSendablexyYTlF : $@convention(method) <T> (Struct) -> @sil_sending @out T {
  func protGenericReturnsNonSendable<T>() -> transferring T { fatalError() }
}
