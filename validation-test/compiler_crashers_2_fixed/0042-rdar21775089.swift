// RUN: %target-swift-frontend %s -emit-ir

struct MySlice<Base : MyIndexableType> : MyCollectionType {}
struct MyMutableSlice<Base : MyMutableCollectionType> : MyMutableCollectionType {}

protocol MySequenceType {}
protocol MyIndexableType {}

protocol MyCollectionType : MySequenceType, MyIndexableType {
  typealias SubSequence = MySlice<Self>
  func makeSubSequence() -> SubSequence
}
extension MyCollectionType {
  func makeSubSequence() -> MySlice<Self> {
    typealias S = Self
    return MySlice<S>()
  }
}

protocol MyMutableCollectionType : MyCollectionType {
  typealias SubSequence = MyMutableSlice<Self>
}
extension MyMutableCollectionType {
  func makeSubSequence() -> MyMutableSlice<Self> {
    typealias S = Self
    return MyMutableSlice<S>()
  }
}

