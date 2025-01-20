// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-experimental-feature KeyPathWithMethodMembers -typecheck -verify %s
// REQUIRES: objc_interop
// REQUIRES: swift_feature_KeyPathWithMethodMembers

import Foundation

@objc class A : NSObject {
  func uniqueID() -> Int {
    42
  }
}

// FIXME: the diagnostic below ideally should have been emitted (rdar://106241733)
struct Loop< 
// expected-note@-1 {{required by generic struct 'Loop' where 'ID' = '() -> Int'}}
  Data : RandomAccessCollection,
  ID : Hashable,
  Content
> {
  public init(
    _ data: Data,
    id: KeyPath<Data.Element, ID>,
    content: @escaping (Data.Element) -> Content) {}
}

func data() -> [A] {
  return []
}

_ = Loop(data(), id: \.uniqueID) { $0 } 
// expected-error@-1 {{type '() -> Int' cannot conform to 'Hashable'}}
// expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
