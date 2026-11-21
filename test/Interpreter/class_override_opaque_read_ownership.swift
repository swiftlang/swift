// RUN: %target-run-simple-swift(-enable-experimental-feature Lifetimes) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Lifetimes -O) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Lifetimes -enable-experimental-feature CoroutineAccessors) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_CoroutineAccessors

// rdar://192958686: A class property override has the same opaque read
// accessors as the property it overrides, even if it is implemented with
// different accessors or a different '@_borrowed' attribute. So a read through
// the base dispatches to the override. Each base returns 0 and each override
// returns 1.

// Base and override implemented with getters and '_read' coroutines.

class Base_Get_Get { var p: Int { get { 0 } } }
class Derived_Get_Get: Base_Get_Get { override var p: Int { get { 1 } } }

class Base_Get_BorrowedGet { var p: Int { get { 0 } } }
class Derived_Get_BorrowedGet: Base_Get_BorrowedGet { @_borrowed override var p: Int { get { 1 } } }

class Base_Get_Read { var p: Int { get { 0 } } }
class Derived_Get_Read: Base_Get_Read { override var p: Int { _read { yield 1 } } }

class Base_Get_BorrowedRead { var p: Int { get { 0 } } }
class Derived_Get_BorrowedRead: Base_Get_BorrowedRead { @_borrowed override var p: Int { _read { yield 1 } } }

class Base_BorrowedGet_Get { @_borrowed var p: Int { get { 0 } } }
class Derived_BorrowedGet_Get: Base_BorrowedGet_Get { override var p: Int { get { 1 } } }

class Base_BorrowedGet_BorrowedGet { @_borrowed var p: Int { get { 0 } } }
class Derived_BorrowedGet_BorrowedGet: Base_BorrowedGet_BorrowedGet { @_borrowed override var p: Int { get { 1 } } }

class Base_BorrowedGet_Read { @_borrowed var p: Int { get { 0 } } }
class Derived_BorrowedGet_Read: Base_BorrowedGet_Read { override var p: Int { _read { yield 1 } } }

class Base_BorrowedGet_BorrowedRead { @_borrowed var p: Int { get { 0 } } }
class Derived_BorrowedGet_BorrowedRead: Base_BorrowedGet_BorrowedRead { @_borrowed override var p: Int { _read { yield 1 } } }

class Base_Read_Get { var p: Int { _read { yield 0 } } }
class Derived_Read_Get: Base_Read_Get { override var p: Int { get { 1 } } }

class Base_Read_BorrowedGet { var p: Int { _read { yield 0 } } }
class Derived_Read_BorrowedGet: Base_Read_BorrowedGet { @_borrowed override var p: Int { get { 1 } } }

class Base_Read_Read { var p: Int { _read { yield 0 } } }
class Derived_Read_Read: Base_Read_Read { override var p: Int { _read { yield 1 } } }

class Base_Read_BorrowedRead { var p: Int { _read { yield 0 } } }
class Derived_Read_BorrowedRead: Base_Read_BorrowedRead { @_borrowed override var p: Int { _read { yield 1 } } }

class Base_BorrowedRead_Get { @_borrowed var p: Int { _read { yield 0 } } }
class Derived_BorrowedRead_Get: Base_BorrowedRead_Get { override var p: Int { get { 1 } } }

class Base_BorrowedRead_BorrowedGet { @_borrowed var p: Int { _read { yield 0 } } }
class Derived_BorrowedRead_BorrowedGet: Base_BorrowedRead_BorrowedGet { @_borrowed override var p: Int { get { 1 } } }

class Base_BorrowedRead_Read { @_borrowed var p: Int { _read { yield 0 } } }
class Derived_BorrowedRead_Read: Base_BorrowedRead_Read { override var p: Int { _read { yield 1 } } }

class Base_BorrowedRead_BorrowedRead { @_borrowed var p: Int { _read { yield 0 } } }
class Derived_BorrowedRead_BorrowedRead: Base_BorrowedRead_BorrowedRead { @_borrowed override var p: Int { _read { yield 1 } } }

// Stored base property overridden with a getter and setter.
class Base_Stored_GetSet { var p: Int = 0 }
class Derived_Stored_GetSet: Base_Stored_GetSet { @_borrowed override var p: Int { get { 1 } set {} } }

class Base_BorrowedStored_GetSet { @_borrowed var p: Int = 0 }
class Derived_BorrowedStored_GetSet: Base_BorrowedStored_GetSet { override var p: Int { get { 1 } set {} } }

// A 'borrow' accessor overriding a borrowed read.
class Base_BorrowedGet_Borrow { @_borrowed var p: Int { get { 0 } } }
class Derived_BorrowedGet_Borrow: Base_BorrowedGet_Borrow {
  let one = 1
  override var p: Int { borrow { return one } }
}

// A non-Escapable property whose override is implemented with a different
// accessor, with the same lifetime dependence.
struct NE: ~Escapable {
  var value: Int
  @_lifetime(immortal) init(_ value: Int) { self.value = value }
}

class Base_ScopedRead_ScopedGet {
  var p: NE { @_lifetime(borrow self) _read { yield NE(0) } }
}
class Derived_ScopedRead_ScopedGet: Base_ScopedRead_ScopedGet {
  override var p: NE { @_lifetime(borrow self) get { NE(1) } }
}

class Base_ImmortalRead_ImmortalGet {
  var p: NE { @_lifetime(immortal) _read { yield NE(0) } }
}
class Derived_ImmortalRead_ImmortalGet: Base_ImmortalRead_ImmortalGet {
  override var p: NE { @_lifetime(immortal) get { NE(1) } }
}

class Base_BorrowedImmortalGet_ImmortalGet {
  @_borrowed var p: NE { @_lifetime(immortal) get { NE(0) } }
}
class Derived_BorrowedImmortalGet_ImmortalGet: Base_BorrowedImmortalGet_ImmortalGet {
  override var p: NE { @_lifetime(immortal) get { NE(1) } }
}

class Base_CopyRead_CopyGet {
  subscript(ne: NE) -> NE { @_lifetime(copy ne) _read { yield NE(0) } }
}
class Derived_CopyRead_CopyGet: Base_CopyRead_CopyGet {
  override subscript(ne: NE) -> NE { @_lifetime(copy ne) get { NE(1) } }
}

// CHECK: Get_Get: 1
print("Get_Get: \((Derived_Get_Get() as Base_Get_Get).p)")
// CHECK: Get_BorrowedGet: 1
print("Get_BorrowedGet: \((Derived_Get_BorrowedGet() as Base_Get_BorrowedGet).p)")
// CHECK: Get_Read: 1
print("Get_Read: \((Derived_Get_Read() as Base_Get_Read).p)")
// CHECK: Get_BorrowedRead: 1
print("Get_BorrowedRead: \((Derived_Get_BorrowedRead() as Base_Get_BorrowedRead).p)")
// CHECK: BorrowedGet_Get: 1
print("BorrowedGet_Get: \((Derived_BorrowedGet_Get() as Base_BorrowedGet_Get).p)")
// CHECK: BorrowedGet_BorrowedGet: 1
print("BorrowedGet_BorrowedGet: \((Derived_BorrowedGet_BorrowedGet() as Base_BorrowedGet_BorrowedGet).p)")
// CHECK: BorrowedGet_Read: 1
print("BorrowedGet_Read: \((Derived_BorrowedGet_Read() as Base_BorrowedGet_Read).p)")
// CHECK: BorrowedGet_BorrowedRead: 1
print("BorrowedGet_BorrowedRead: \((Derived_BorrowedGet_BorrowedRead() as Base_BorrowedGet_BorrowedRead).p)")
// CHECK: Read_Get: 1
print("Read_Get: \((Derived_Read_Get() as Base_Read_Get).p)")
// CHECK: Read_BorrowedGet: 1
print("Read_BorrowedGet: \((Derived_Read_BorrowedGet() as Base_Read_BorrowedGet).p)")
// CHECK: Read_Read: 1
print("Read_Read: \((Derived_Read_Read() as Base_Read_Read).p)")
// CHECK: Read_BorrowedRead: 1
print("Read_BorrowedRead: \((Derived_Read_BorrowedRead() as Base_Read_BorrowedRead).p)")
// CHECK: BorrowedRead_Get: 1
print("BorrowedRead_Get: \((Derived_BorrowedRead_Get() as Base_BorrowedRead_Get).p)")
// CHECK: BorrowedRead_BorrowedGet: 1
print("BorrowedRead_BorrowedGet: \((Derived_BorrowedRead_BorrowedGet() as Base_BorrowedRead_BorrowedGet).p)")
// CHECK: BorrowedRead_Read: 1
print("BorrowedRead_Read: \((Derived_BorrowedRead_Read() as Base_BorrowedRead_Read).p)")
// CHECK: BorrowedRead_BorrowedRead: 1
print("BorrowedRead_BorrowedRead: \((Derived_BorrowedRead_BorrowedRead() as Base_BorrowedRead_BorrowedRead).p)")
// CHECK: Stored_GetSet: 1
print("Stored_GetSet: \((Derived_Stored_GetSet() as Base_Stored_GetSet).p)")
// CHECK: BorrowedStored_GetSet: 1
print("BorrowedStored_GetSet: \((Derived_BorrowedStored_GetSet() as Base_BorrowedStored_GetSet).p)")
// CHECK: BorrowedGet_Borrow: 1
print("BorrowedGet_Borrow: \((Derived_BorrowedGet_Borrow() as Base_BorrowedGet_Borrow).p)")
// CHECK: ScopedRead_ScopedGet: 1
print("ScopedRead_ScopedGet: \((Derived_ScopedRead_ScopedGet() as Base_ScopedRead_ScopedGet).p.value)")
// CHECK: ImmortalRead_ImmortalGet: 1
print("ImmortalRead_ImmortalGet: \((Derived_ImmortalRead_ImmortalGet() as Base_ImmortalRead_ImmortalGet).p.value)")
// CHECK: BorrowedImmortalGet_ImmortalGet: 1
print("BorrowedImmortalGet_ImmortalGet: \((Derived_BorrowedImmortalGet_ImmortalGet() as Base_BorrowedImmortalGet_ImmortalGet).p.value)")
// CHECK: CopyRead_CopyGet: 1
print("CopyRead_CopyGet: \((Derived_CopyRead_CopyGet() as Base_CopyRead_CopyGet)[NE(0)].value)")
