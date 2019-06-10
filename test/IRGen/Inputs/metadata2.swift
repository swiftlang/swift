import resilient_struct

struct Item {
 var d: ResilientInt? = nil
}

struct InternalContainer {

    fileprivate enum SomeEnumType {
        case none
        case single(Item)

        init(item: [Item]) {
            if item.count >= 1 {
                self = .single(item.first!)
            } else {
                self = .none
            }
        }
    }
    private var type: SomeEnumType

    init(item: [Item]) {
        self.type = SomeEnumType(item: item)
    }
}

struct InternalContainer2 {

    fileprivate enum SomeEnumType {
        case none
        case single(Item)

        init(item: [Item]) {
            if item.count >= 1 {
                self = .single(item.first!)
            } else {
                self = .none
            }
        }
    }

    private var type: (SomeEnumType, SomeEnumType)

    init(item: [Item]) {
        self.type = SomeEnumType(item: item)
    }
}

enum InternalSingletonEnum {
  fileprivate enum SomeEnumType {
       case none
       case single(Item)

       init(item: [Item]) {
           if item.count >= 1 {
               self = .single(item.first!)
           } else {
               self = .none
           }
       }
  }
  case first(SomeEnumType)

  init() {
    return .first(.none)
  }
}

enum InternalSingletonEnum2 {
  fileprivate enum SomeEnumType {
       case none
       case single(Item)

       init(item: [Item]) {
           if item.count >= 1 {
               self = .single(item.first!)
           } else {
               self = .none
           }
       }
  }

  case first(SomeEnumType, SomeEnumType)

  init() {
    return .first(.none, .none)
  }
}
