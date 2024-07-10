// RUN: %target-typecheck-verify-swift

protocol Idable<ID> {
  associatedtype ID
  var id: ID { get }
}

@dynamicMemberLookup struct Binding<C> {
  subscript(dynamicMember member: String) -> String { "" }
}

struct ForEach<Data, ID, Content> where Data : RandomAccessCollection, ID: Hashable {
  init(_ data: Data, content: (Data.Element) -> Content) where ID == Data.Element.ID, Data.Element : Idable {}
  // expected-note@-1 {{where 'Data.Element' = 'User'}}
  init<C>(_ data: Binding<C>, content: (Binding<C.Element>) -> Content) where Data == Array<C>, ID == C.Element.ID, C : MutableCollection, C : RandomAccessCollection, C.Element : Idable, C.Index : Hashable {}
}

struct User {
  let name: String // expected-note 3 {{'name' declared here}}
  func member() {}
}

struct ContentView {
  let users: [User] = []
  func body() -> Void {
    ForEach(users) { user in // expected-error {{initializer 'init(_:content:)' requires that 'User' conform to 'Idable'}}
      return user.nam // expected-error {{value of type 'User' has no member 'nam'; did you mean 'name'?}}
    }
  }
}


@dynamicMemberLookup struct BindingB {
  subscript(dynamicMember member: String) -> String { "" }
}
func test(_: String) {} // expected-note {{candidate expects value of type 'String' for parameter #1 (got 'Int')}}
func test(_: BindingB) {} // expected-note {{candidate expects value of type 'BindingB' for parameter #1 (got 'Int')}}
test(42) // expected-error {{no exact matches in call to global function 'test'}}


func passThrough<T>(_ :T, _: (T) -> Void) {}
func passThrough<T>(_ :Binding<T>, _: (Binding<T>) -> Void) {}
func passThrough2<T>(_ : T, _: (T) -> Void) {}
func takeString(_: String) {}
func two<T>(_: T, _: T, _: (T) -> Void) {}
func two<T>(_: Binding<T>, _: Binding<T>, _: (Binding<T>) -> Void) {}

func f(s: String, i: Int, u: User, b: Binding<User>) {
  passThrough(u) {
    takeString($0.nam) // expected-error {{value of type 'User' has no member 'nam'}}
    passThrough(b) { takeString($0.a) }
  }
  passThrough(u) {
    passThrough2($0) { takeString($0.nam) }  // expected-error {{value of type 'User' has no member 'nam'}}
  }
  passThrough(b) {
    takeString($0.name)
    passThrough(s) { takeString($0) }
    passThrough(i) { takeString($0) } // expected-error {{cannot convert value of type 'Int' to expected argument type 'String'}}
  }

  two(u, b) { takeString($0.b) } // expected-error {{cannot convert value of type 'User' to expected argument type 'Binding<User>'}}
  two(u, b) { $0.member() } // expected-error {{cannot convert value of type 'Binding<User>' to expected argument type 'User'}}
}
