import Repo1

public extension MyThing {
  enum MyEnum {
    case mycase

    private struct MyPrivateThing : Q {
      init() {
      }

      var thing: some Q {
        return self
      }
    }

    private var data: MyPrivateThing {
      switch self {
      case .mycase: return MyPrivateThing()
      }
    }

    public var thing: some Q {
      self.data.thing
    }
  }
}
