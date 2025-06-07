// RUN: %batch-code-completion

public struct ItemWrapper {
  let content: Int
}

struct MyArray {
  func map(transform: (ItemWrapper) -> Int) {}
}

func sink(receiveValue: (MyArray) -> Void) {
  fatalError()
}

func foo() {
  sink { items in
    let a = items.#^COMPLETE_WITHOUT_SPACE?check=CHECK^#map{ $0.content }
  }
  sink { items in
    let b = items.#^COMPLETE_WITH_SPACE?check=CHECK^# map{ $0.content }
  }
  sink { items in
    let c = items.#^COMPLETE_WITH_SPACE_AND_PARENS?check=CHECK^# map({ $0.content })
  }
  sink { items in
    let d = items.#^COMPLETE_WITHOUT_SPACE_BUT_PARENS?check=CHECK^#map({ $0.content })
  }
  sink { items in
    let e = items.#^COMPLETE_WITHOUT_MAP?check=CHECK^# { $0.content }
  }
  sink { items in
    let f = items.#^COMPLETE_WITHOUT_MAP_BUT_PARENS?check=CHECK^# ({ $0.content })
  }
}

// CHECK: Begin completions, 2 items
// CHECK-DAG: Keyword[self]/CurrNominal:          self[#MyArray#];
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: map({#transform: (ItemWrapper) -> Int##(ItemWrapper) -> Int#})[#Void#];
