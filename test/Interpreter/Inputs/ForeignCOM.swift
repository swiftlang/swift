import ForeignCOM

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IValue {
  func value(_ offset: Int32) -> Int32
}

@com(interface: "10000000-0000-0000-0000-000000000002")
protocol IExtended: IValue {
  func multiply(_ factor: Int32) -> Int32
}

@com(interface: "10000000-0000-0000-0000-000000000003")
protocol IProperty: AnyObject {
  var value: Int32 { get set }
  subscript(_ index: Int32) -> Int32 { get }
  func reset()
}

@inline(never)
func makeValue(_ value: Int32) -> any IValue {
  let object = ForeignCOMObject_Create(value)!
  let storage = ForeignCOMObject_GetValueStorage(object)!
  let interface = storage.load(as: (any IValue).self)
  ForeignCOMObject_Release(object)
  return interface
}

@inline(never)
func makeExtended(_ value: Int32) -> any IExtended {
  let object = ForeignCOMObject_Create(value)!
  let storage = ForeignCOMObject_GetValueStorage(object)!
  let interface = storage.load(as: (any IExtended).self)
  ForeignCOMObject_Release(object)
  return interface
}

@inline(never)
func makeProperty(_ value: Int32) -> any IProperty {
  let object = ForeignCOMObject_Create(value)!
  let storage = ForeignCOMObject_GetPropertyStorage(object)!
  let interface = storage.load(as: (any IProperty).self)
  ForeignCOMObject_Release(object)
  return interface
}

func checkDestruction() {
  precondition(GetForeignCOMReferenceCount() == 0)
  precondition(GetForeignCOMDestructionCount() == 1)
  // The factory supplies the initial reference; every additional reference
  // must be balanced independently of optimization-dependent copy counts.
  precondition(GetForeignCOMReleaseCalls() == GetForeignCOMAddRefCalls() + 1)
}
