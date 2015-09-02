// RUN: %target-parse-verify-swift

struct MyCollection<Element> {
  func map<T>(transform: (Element) -> T) -> MyCollection<T> {
    fatalError("implement")
  }
}

MyCollection.map // expected-error{{generic parameter 'Element' could not be inferred}}

let a = MyCollection<Int>()
a.map // expected-error{{generic parameter 'T' could not be inferred}}

