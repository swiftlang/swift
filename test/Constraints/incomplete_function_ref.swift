// RUN: %target-parse-verify-swift

struct MyCollection<Element> {  // expected-note {{'Element' declared as parameter to type 'MyCollection'}}
  func map<T>(_ transform: (Element) -> T) -> MyCollection<T> {
    fatalError("implement")
  }
}

MyCollection.map // expected-error{{generic parameter 'Element' could not be inferred}}

let a = MyCollection<Int>()
a.map // expected-error{{generic parameter 'T' could not be inferred}}

