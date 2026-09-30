// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -emit-module-path %t/COM.swiftmodule -module-name COM %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -sil-verify-all -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -sil-verify-all -emit-sil %s -o /dev/null

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IBase {
  func method(_ value: CInt) -> CInt
}

@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol IDerived: IBase {
  func derived(_ value: CInt) -> CInt
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}4base
// CHECK:         [[OPEN:%.*]] = open_com_existential {{%.*}} to $@opened{{.*}}IBase
// CHECK:         [[STORAGE:%.*]] = alloc_stack $@opened
// CHECK:         [[BORROW:%.*]] = store_borrow [[OPEN]] to [[STORAGE]]
// CHECK:         [[METHOD:%.*]] = com_method [[BORROW]], #IBase.method
// CHECK-SAME:    $@convention(com_method)
// CHECK-SAME:    type-defs: [[OPEN]];
// CHECK-NOT:     = witness_method
// CHECK:         apply [[METHOD]]<{{.*}}>({{%.*}}, [[BORROW]])
// CHECK:         end_borrow [[BORROW]]
// CHECK:         dealloc_stack [[STORAGE]]
// CHECK:         return
public func base(_ interface: borrowing any IBase, _ value: CInt) -> CInt {
  interface.method(value)
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}7refined
// CHECK:         [[OPEN:%.*]] = open_com_existential {{%.*}} to $@opened{{.*}}IDerived
// CHECK:         [[STORAGE:%.*]] = alloc_stack $@opened
// CHECK:         [[BORROW:%.*]] = store_borrow [[OPEN]] to [[STORAGE]]
// CHECK:         [[METHOD:%.*]] = com_method [[BORROW]], #IBase.method
// CHECK-SAME:    $@convention(com_method)
// CHECK-SAME:    type-defs: [[OPEN]];
// CHECK-NOT:     = witness_method
// CHECK:         apply [[METHOD]]<{{.*}}>({{%.*}}, [[BORROW]])
// CHECK:         end_borrow [[BORROW]]
// CHECK:         dealloc_stack [[STORAGE]]
// CHECK:         return
public func refined(_ interface: borrowing any IDerived, _ value: CInt) -> CInt {
  interface.method(value)
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}7derived
// CHECK:         [[OPEN:%.*]] = open_com_existential {{%.*}} to $@opened{{.*}}IDerived
// CHECK:         [[STORAGE:%.*]] = alloc_stack $@opened
// CHECK:         [[BORROW:%.*]] = store_borrow [[OPEN]] to [[STORAGE]]
// CHECK:         [[METHOD:%.*]] = com_method [[BORROW]], #IDerived.derived
// CHECK-SAME:    $@convention(com_method)
// CHECK-SAME:    type-defs: [[OPEN]];
// CHECK-NOT:     = witness_method
// CHECK:         apply [[METHOD]]<{{.*}}>({{%.*}}, [[BORROW]])
// CHECK:         end_borrow [[BORROW]]
// CHECK:         dealloc_stack [[STORAGE]]
// CHECK:         return
public func derived(_ interface: borrowing any IDerived, _ value: CInt) -> CInt {
  interface.derived(value)
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}5owned
// CHECK:         [[VALUE:%.*]] = load [copy]
// CHECK:         [[OPEN:%.*]] = open_com_existential [[VALUE]] to $@opened{{.*}}IBase
// CHECK:         [[STORAGE:%.*]] = alloc_stack $@opened
// CHECK:         store [[OPEN]] to [init] [[STORAGE]]
// CHECK:         [[METHOD:%.*]] = com_method [[STORAGE]], #IBase.method
// CHECK-SAME:    $@convention(com_method)
// CHECK-SAME:    type-defs: [[OPEN]];
// CHECK:         apply [[METHOD]]<{{.*}}>({{%.*}}, [[STORAGE]])
// CHECK:         destroy_addr [[STORAGE]]
// CHECK:         dealloc_stack [[STORAGE]]
// CHECK:         return
public func owned(_ interface: consuming any IBase, _ value: CInt) -> CInt {
  interface.method(value)
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}13inoutReceiver
// CHECK:         [[VALUE:%.*]] = load [copy]
// CHECK:         [[OPEN:%.*]] = open_com_existential [[VALUE]] to $@opened{{.*}}IBase
// CHECK:         [[STORAGE:%.*]] = alloc_stack $@opened
// CHECK:         store [[OPEN]] to [init] [[STORAGE]]
// CHECK:         [[METHOD:%.*]] = com_method [[STORAGE]], #IBase.method
// CHECK-SAME:    $@convention(com_method)
// CHECK-SAME:    type-defs: [[OPEN]];
// CHECK:         apply [[METHOD]]<{{.*}}>({{%.*}}, [[STORAGE]])
// CHECK:         destroy_addr [[STORAGE]]
// CHECK:         dealloc_stack [[STORAGE]]
// CHECK:         return
public func inoutReceiver(_ interface: inout any IBase, _ value: CInt) -> CInt {
  interface.method(value)
}

@com(interface: "10000000-0000-0000-0000-000000000003")
public protocol IProperties: AnyObject {
  var value: CInt { get set }
  subscript(_ index: CInt) -> CInt { get }
  func reset()
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}12readProperty
// CHECK:         [[OPEN:%.*]] = open_com_existential {{%.*}} to $@opened{{.*}}IProperties
// CHECK:         [[COPY:%.*]] = copy_value [[OPEN]]
// CHECK:         [[SELF:%.*]] = begin_borrow [[COPY]]
// CHECK:         [[METHOD:%.*]] = com_method [[SELF]], #IProperties.value!getter
// CHECK-SAME:    $@convention(com_method)
// CHECK-SAME:    @guaranteed
// CHECK-SAME:    type-defs: [[OPEN]];
// CHECK:         apply [[METHOD]]<{{.*}}>([[SELF]])
// CHECK:         end_borrow [[SELF]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         return
public func readProperty(_ interface: borrowing any IProperties) -> CInt {
  interface.value
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}13writeProperty
// CHECK:         [[OPEN:%.*]] = open_com_existential {{%.*}} to $@opened{{.*}}IProperties
// CHECK:         [[COPY:%.*]] = copy_value [[OPEN]]
// CHECK:         [[METHOD:%.*]] = com_method [[COPY]], #IProperties.value!setter
// CHECK-SAME:    $@convention(com_method)
// CHECK-SAME:    @guaranteed
// CHECK-SAME:    type-defs: [[OPEN]];
// CHECK:         apply [[METHOD]]<{{.*}}>({{%.*}}, [[COPY]])
// CHECK:         destroy_value [[COPY]]
// CHECK:         return
public func writeProperty(_ interface: borrowing any IProperties, _ value: CInt) {
  interface.value = value
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}13readSubscript
// CHECK:         [[OPEN:%.*]] = open_com_existential {{%.*}} to $@opened{{.*}}IProperties
// CHECK:         [[COPY:%.*]] = copy_value [[OPEN]]
// CHECK:         [[SELF:%.*]] = begin_borrow [[COPY]]
// CHECK:         [[METHOD:%.*]] = com_method [[SELF]], #IProperties.subscript!getter
// CHECK-SAME:    $@convention(com_method)
// CHECK-SAME:    @guaranteed
// CHECK-SAME:    type-defs: [[OPEN]];
// CHECK:         apply [[METHOD]]<{{.*}}>({{%.*}}, [[SELF]])
// CHECK:         end_borrow [[SELF]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         return
public func readSubscript(_ interface: borrowing any IProperties, _ index: CInt) -> CInt {
  interface[index]
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}13discardResult
// CHECK:         [[OPEN:%.*]] = open_com_existential {{%.*}} to $@opened{{.*}}IProperties
// CHECK:         [[METHOD:%.*]] = com_method [[OPEN]], #IProperties.reset
// CHECK-SAME:    $@convention(com_method)
// CHECK-SAME:    (@guaranteed {{.*}}) -> ()
// CHECK-SAME:    type-defs: [[OPEN]];
// CHECK:         apply [[METHOD]]<{{.*}}>([[OPEN]])
// CHECK:         return
public func discardResult(_ interface: borrowing any IProperties) {
  interface.reset()
}

// Requirements imported from the COM module use the same dispatch path.
// CHECK-LABEL: sil [ossa] @$s{{.*}}8imported
// CHECK:         [[OPEN:%.*]] = open_com_existential {{%.*}} to $@opened{{.*}}ISwiftObject
// CHECK:         [[STORAGE:%.*]] = alloc_stack $@opened
// CHECK:         [[BORROW:%.*]] = store_borrow [[OPEN]] to [[STORAGE]]
// CHECK:         [[METHOD:%.*]] = com_method [[BORROW]], #ISwiftObject.object!getter
// CHECK-SAME:    $@convention(com_method)
// CHECK-SAME:    -> UnsafeMutableRawPointer
// CHECK-SAME:    type-defs: [[OPEN]];
// CHECK:         apply [[METHOD]]<{{.*}}>([[BORROW]])
// CHECK:         end_borrow [[BORROW]]
// CHECK:         dealloc_stack [[STORAGE]]
// CHECK:         return
public func imported(_ interface: borrowing any ISwiftObject) -> UnsafeMutableRawPointer {
  interface.object
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}7generic
// CHECK-NOT:     open_com_existential
// CHECK-NOT:     = com_method
// CHECK:         [[METHOD:%.*]] = witness_method $T, #IBase.method
// CHECK-SAME:    $@convention(witness_method: IBase)
// CHECK-NOT:     = com_method
// CHECK:         apply [[METHOD]]<T>
// CHECK-NOT:     = com_method
// CHECK:         return
public func generic<T: IBase>(_ interface: borrowing T, _ value: CInt) -> CInt {
  interface.method(value)
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}14genericDerived
// CHECK-NOT:     open_com_existential
// CHECK-NOT:     = com_method
// CHECK:         [[METHOD:%.*]] = witness_method $T, #IBase.method
// CHECK-SAME:    $@convention(witness_method: IBase)
// CHECK-NOT:     = com_method
// CHECK:         apply [[METHOD]]<T>
// CHECK-NOT:     = com_method
// CHECK:         return
public func genericDerived<T: IDerived>(_ interface: borrowing T, _ value: CInt) -> CInt {
  interface.method(value)
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}12genericClass
// CHECK-NOT:     open_com_existential
// CHECK-NOT:     = com_method
// CHECK:         [[METHOD:%.*]] = witness_method $T, #IProperties.value!getter
// CHECK-SAME:    $@convention(witness_method: IProperties)
// CHECK-NOT:     = com_method
// CHECK:         apply [[METHOD]]<T>
// CHECK-NOT:     = com_method
// CHECK:         return
public func genericClass<T: IProperties>(_ interface: borrowing T) -> CInt {
  interface.value
}

// Protocol extension helpers are native methods. Their abstract Self receiver
// still dispatches requirements through a Swift witness table.
// CHECK-LABEL: sil [ossa] @$s{{.*}}5IBaseP{{.*}}6helper
// CHECK-SAME:    $@convention(method)
// CHECK-NOT:     = com_method
// CHECK:         [[METHOD:%.*]] = witness_method $Self, #IBase.method
// CHECK-SAME:    $@convention(witness_method: IBase)
// CHECK:         apply [[METHOD]]<Self>
// CHECK-NOT:     = com_method
// CHECK:         return
extension IBase {
  public func helper(_ value: CInt) -> CInt {
    method(value)
  }
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}15extensionMethod
// CHECK:         open_com_existential
// CHECK-NOT:     = com_method
// CHECK:         [[HELPER:%.*]] = function_ref @$s{{.*}}5IBaseP{{.*}}6helper
// CHECK-SAME:    $@convention(method)
// CHECK:         apply [[HELPER]]<{{.*}}>
// CHECK-NOT:     = com_method
// CHECK:         return
public func extensionMethod(_ interface: borrowing any IBase, _ value: CInt) -> CInt {
  interface.helper(value)
}

// Concrete implementations and their witness thunks retain Swift conventions.
// CHECK-LABEL: sil [ossa] @$s{{.*}}6NativeC6method
// CHECK-SAME:    $@convention(method)
public final class Native: IBase {
  public func method(_ value: CInt) -> CInt { value }
}

// CHECK-LABEL: sil shared {{.*}} [thunk] [ossa] @$s{{.*}}6NativeC{{.*}}TW
// CHECK-SAME:    $@convention(witness_method: IBase)
// CHECK-NOT:     = com_method
// CHECK:         [[METHOD:%.*]] = function_ref @$s{{.*}}6NativeC6method
// CHECK-SAME:    $@convention(method)
// CHECK:         apply [[METHOD]]
// CHECK-NOT:     = com_method
// CHECK:         return

// CHECK-LABEL: sil [ossa] @$s{{.*}}8concrete
// CHECK-NOT:     = com_method
// CHECK:         [[METHOD:%.*]] = function_ref @$s{{.*}}6NativeC6method
// CHECK-SAME:    $@convention(method)
// CHECK:         apply [[METHOD]]
// CHECK-NOT:     = com_method
// CHECK:         return
public func concrete(_ interface: Native, _ value: CInt) -> CInt {
  interface.method(value)
}

public protocol SwiftInterface {
  func method(_ value: CInt) -> CInt
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}16swiftExistential
// CHECK-NOT:     open_com_existential
// CHECK:         open_existential_addr
// CHECK-NOT:     = com_method
// CHECK:         [[METHOD:%.*]] = witness_method $@opened{{.*}}, #SwiftInterface.method
// CHECK-SAME:    $@convention(witness_method: SwiftInterface)
// CHECK:         apply [[METHOD]]<{{.*}}>
// CHECK-NOT:     = com_method
// CHECK:         return
public func swiftExistential(_ interface: any SwiftInterface, _ value: CInt) -> CInt {
  interface.method(value)
}

// The factory's owned result is forwarded through the opening into the temporary
// receiver storage, whose cleanup runs after the call.
// CHECK-LABEL: sil [ossa] @$s{{.*}}9temporary
// CHECK:         [[VALUE:%.*]] = apply {{%.*}}()
// CHECK-SAME:    -> @owned any IBase
// CHECK:         [[OPEN:%.*]] = open_com_existential [[VALUE]] to $@opened{{.*}}IBase
// CHECK:         [[STORAGE:%.*]] = alloc_stack $@opened
// CHECK:         store [[OPEN]] to [init] [[STORAGE]]
// CHECK:         [[METHOD:%.*]] = com_method [[STORAGE]], #IBase.method
// CHECK-SAME:    $@convention(com_method)
// CHECK-SAME:    type-defs: [[OPEN]];
// CHECK:         apply [[METHOD]]<{{.*}}>({{%.*}}, [[STORAGE]])
// CHECK:         destroy_addr [[STORAGE]]
// CHECK:         dealloc_stack [[STORAGE]]
// CHECK:         return
public func temporary(_ factory: () -> any IBase, _ value: CInt) -> CInt {
  factory().method(value)
}

// A bound method captures the existential and opens it when the closure runs.
// CHECK-LABEL: sil [ossa] @$s{{.*}}5bound
// CHECK-NOT:     open_com_existential
// CHECK:         [[MAKE:%.*]] = function_ref @$s{{.*}}5bound{{.*}}cfu_
// CHECK:         apply [[MAKE]]
// CHECK-NOT:     open_com_existential
// CHECK:         return
public func bound(_ interface: any IBase) -> (CInt) -> CInt {
  interface.method
}

// CHECK-LABEL: sil private [ossa] @$s{{.*}}5bound{{.*}}cfu_ :
// CHECK:         [[BODY:%.*]] = function_ref @$s{{.*}}5bound{{.*}}cfu0_
// CHECK:         [[CAPTURE:%.*]] = copy_value
// CHECK:         partial_apply [callee_guaranteed] [[BODY]]([[CAPTURE]])
// CHECK:         return

// CHECK-LABEL: sil private [ossa] @$s{{.*}}5bound{{.*}}cfu0_ :
// CHECK:         [[OPEN:%.*]] = open_com_existential {{%.*}} to $@opened{{.*}}IBase
// CHECK:         [[STORAGE:%.*]] = alloc_stack $@opened
// CHECK:         [[BORROW:%.*]] = store_borrow [[OPEN]] to [[STORAGE]]
// CHECK:         [[METHOD:%.*]] = com_method [[BORROW]], #IBase.method
// CHECK-SAME:    $@convention(com_method)
// CHECK-SAME:    type-defs: [[OPEN]];
// CHECK:         apply [[METHOD]]<{{.*}}>({{%.*}}, [[BORROW]])
// CHECK:         end_borrow [[BORROW]]
// CHECK:         dealloc_stack [[STORAGE]]
// CHECK:         return
