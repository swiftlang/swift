// RUN: %target-typecheck-verify-swift

@propertyWrapper
struct SomeStruct<T> {
  var wrappedValue: T 
    
  init(wrappedValue: T) { 
    self.wrappedValue = wrappedValue
  } 
}

typealias Invalid1<T> = (T) -> ()
typealias Invalid2<T> = SomeStruct<T>.Type
typealias Invalid3 = SomeStruct<Bool>.Type

struct Bar {
  @Invalid1 var badWrapper1: Bool = false // expected-error {{unknown attribute 'Invalid1'}}
  @Invalid2 var badWrapper2: Bool = false // expected-error {{unknown attribute 'Invalid2'}}
  @Invalid3 var badWrapper3: Bool = false // expected-error {{unknown attribute 'Invalid3'}}
}
