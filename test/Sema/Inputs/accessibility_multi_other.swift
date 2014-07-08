private(set) var privateSetGlobal = 0

struct Members {
  private(set) var privateSetProp = 0
  private(set) subscript() -> Int {
    get { return 0 }
    set {}
  }
}
