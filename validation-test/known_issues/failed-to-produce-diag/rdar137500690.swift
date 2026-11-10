// rdar://137500690
// RUN: %target-typecheck-verify-swift

protocol BackingData<Model> {
  associatedtype Model
}

class DataItem {
  init() {}
  required init(backingData: any BackingData<DataItem>) {}
}

func test(item: DataItem) {
  _ = DataItem(backingData: item as! any BackingData) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
