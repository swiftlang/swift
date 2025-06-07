func test() {
  struct MyStruct {
// RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):16 %s -- %s | %FileCheck %s
    @State var myState
  }
}

@propertyWrapper struct State {
    init(initialValue value: Int) {}

    public var wrappedValue: Int {
        get { return 1 }
        nonmutating set { }
    }
}

// CHECK: <Declaration>@<Type usr="s:46cursor_infer_nonmutating_from_property_wrapper5StateV">State</Type> var myState: &lt;&lt;error type&gt;&gt; { get nonmutating set }</Declaration>
