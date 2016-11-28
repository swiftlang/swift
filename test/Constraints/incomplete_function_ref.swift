// RUN: %target-typecheck-verify-swift

struct MyCollection<Element> {  // expected-note {{'Element' declared as parameter to type 'MyCollection'}}
  func map<T>(_ transform: (Element) -> T) -> MyCollection<T> {
    fatalError("implement")
  }
}

MyCollection.map // expected-error{{generic parameter 'Element' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{13-13=<Any>}}

let a = MyCollection<Int>()
a.map // expected-error{{generic parameter 'T' could not be inferred}}

