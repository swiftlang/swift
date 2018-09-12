// RUN: %target-swift-emit-silgen -enable-sil-ownership %s -o /dev/null -verify
// RUN: %target-swift-emit-silgen -enable-sil-ownership -enforce-exclusivity=checked %s -o /dev/null -verify

func takeInOut<T>(_: inout T) {}

struct MutatorStruct {
  mutating func f(_ x : inout MutatorStruct) {}
}

var global_property : MutatorStruct { get {} set {} }

var global_int_property : Int {
get { return 42 }
set {}
}


struct StructWithProperty {
  var computed_int : Int {
  get { return 42 }
  set {}
  }
  
  var stored_int = 0
  
  var computed_struct : MutatorStruct { get {} set {} }
}

var global_struct_property : StructWithProperty
var c_global_struct_property : StructWithProperty { get {} set {} }

func testInOutAlias() {
  var x = 42
  swap(&x,  // expected-note {{previous aliasing argument}}
       &x)  // expected-error {{inout arguments are not allowed to alias each other}}
  swap(&global_struct_property,  // expected-note {{previous aliasing argument}}
       &global_struct_property)  // expected-error {{inout arguments are not allowed to alias each other}}
}

func testWriteback() {
  var a = StructWithProperty()
  a.computed_struct .        // expected-note {{concurrent writeback occurred here}}
     f(&a.computed_struct)  // expected-error {{inout writeback to computed property 'computed_struct' occurs in multiple arguments to call, introducing invalid aliasing}}
  
  swap(&global_struct_property.stored_int,
       &global_struct_property.stored_int) // ok
  swap(&global_struct_property.computed_int,  // expected-note {{concurrent writeback occurred here}}
       &global_struct_property.computed_int)  // expected-error {{inout writeback to computed property 'computed_int' occurs in multiple arguments to call, introducing invalid aliasing}}
  
  
  
  swap(&a.computed_int,   // expected-note {{concurrent writeback occurred here}}
       &a.computed_int)   // expected-error {{inout writeback to computed property 'computed_int' occurs in multiple arguments to call, introducing invalid aliasing}}
  
  
  global_property.f(&global_property) // expected-error {{inout writeback to computed property 'global_property' occurs in multiple arguments to call, introducing invalid aliasing}} expected-note {{concurrent writeback occurred here}}
  
  a.computed_struct.f(&a.computed_struct)  // expected-error {{inout writeback to computed property 'computed_struct' occurs in multiple arguments to call, introducing invalid aliasing}} expected-note {{concurrent writeback occurred here}}
}

func testComputedStructWithProperty() {
  swap(&c_global_struct_property.stored_int, &c_global_struct_property.stored_int)   // expected-error {{inout writeback to computed property 'c_global_struct_property' occurs in multiple arguments to call, introducing invalid aliasing}} expected-note {{concurrent writeback occurred here}}
  
  var c_local_struct_property : StructWithProperty { get {} set {} }
  swap(&c_local_struct_property.stored_int, &c_local_struct_property.stored_int)    // expected-error {{inout writeback to computed property 'c_local_struct_property' occurs in multiple arguments to call, introducing invalid aliasing}} expected-note {{concurrent writeback occurred here}}
  swap(&c_local_struct_property.stored_int, &c_global_struct_property.stored_int) // ok
}


var global_array : [[Int]]

func testMultiArray(_ i : Int, j : Int, array : [[Int]]) {
  var array = array
  swap(&array[i][j],  // expected-note  {{concurrent writeback occurred here}}
       &array[i][i])  // expected-error {{inout writeback through subscript occurs in multiple arguments to call, introducing invalid aliasing}}
  swap(&array[0][j],  // expected-note  {{concurrent writeback occurred here}}
       &array[0][i])  // expected-error {{inout writeback through subscript occurs in multiple arguments to call, introducing invalid aliasing}}
  swap(&global_array[0][j],  // expected-note  {{concurrent writeback occurred here}}
       &global_array[0][i])  // expected-error {{inout writeback through subscript occurs in multiple arguments to call, introducing invalid aliasing}}
  
  // TODO: This is obviously the same writeback problem, but isn't detectable
  // with the current level of sophistication in SILGen.
  swap(&array[1+0][j], &array[1+0][i])

  swap(&global_array[0][j], &array[j][i])  // ok
}

struct ArrayWithoutAddressors<T> {
  subscript(i: Int) -> T {
    get { return value }
    set {}
  }
  var value: T
}

var global_array_without_addressors: ArrayWithoutAddressors<ArrayWithoutAddressors<Int>>

func testMultiArrayWithoutAddressors(
  _ i: Int, j: Int, array: ArrayWithoutAddressors<ArrayWithoutAddressors<Int>>
) {
  var array = array
  swap(&array[i][j],  // expected-note  {{concurrent writeback occurred here}}
       &array[i][i])  // expected-error {{inout writeback through subscript occurs in multiple arguments to call, introducing invalid aliasing}}
  swap(&array[0][j],  // expected-note  {{concurrent writeback occurred here}}
       &array[0][i])  // expected-error {{inout writeback through subscript occurs in multiple arguments to call, introducing invalid aliasing}}
  swap(&global_array_without_addressors[0][j],   // expected-note  {{concurrent writeback occurred here}}
       &global_array_without_addressors[0][i])   // expected-error {{inout writeback through subscript occurs in multiple arguments to call, introducing invalid aliasing}}

  // TODO: This is obviously the same writeback problem, but isn't detectable
  // with the current level of sophistication in SILGen.
  swap(&array[1+0][j], &array[1+0][i])

  swap(&global_array_without_addressors[0][j], &array[j][i])  // ok
}

// rdar://43802132
struct ArrayWithReadModify<T> {
  init(value: T) { self.property = value }
  var property: T
  subscript(i: Int) -> T {
    _read { yield property }
    _modify { yield &property }
  }
}

func testArrayWithReadModify<T>(array: ArrayWithReadModify<T>) {
  var copy = array
  swap(&copy[0], &copy[1])
  swap(&copy[0], // expected-note {{concurrent writeback occurred here}}
       &copy[0]) // expected-error {{inout writeback through subscript occurs in multiple arguments to call}}
}

// rdar://44147745
func testNestedArrayWithReadModify<T>(array: ArrayWithReadModify<ArrayWithReadModify<T>>) {
  var copy = array
  takeInOut(&copy[0][0])
}
