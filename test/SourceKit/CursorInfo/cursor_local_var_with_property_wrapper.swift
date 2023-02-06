@propertyWrapper struct State {
    public init(initialValue value: Int)
    public var wrappedValue: Int { get nonmutating set }
}

func loadPage() {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):14 %s -- %s
  @State var pageListener: Int
}
