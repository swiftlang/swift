// rdar://126112140
// RUN: %target-typecheck-verify-swift

@propertyWrapper
@dynamicMemberLookup
struct Binding<Value> {
  var wrappedValue: Value
  var projectedValue: Binding<Value> { self }
  init(projectedValue: Binding<Value>) { self = projectedValue }
  subscript<Subject>(
    dynamicMember keyPath: WritableKeyPath<Value, Subject>
  ) -> Binding<Subject> {
    fatalError()
  }
}

struct Region {}

struct CameraPosition {
  static func region(_: Region) -> CameraPosition { fatalError() }
  var region: Region? { fatalError() }
}

protocol View {}

struct Map: View {
  init(coordinateRegion: Binding<Region>, showsUserLocation: Bool = false) {}
}

struct ContentView {
  @Binding var position: CameraPosition

  var body: some View {
    Map(coordinateRegion: $position.region, showsUserLocation: true) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  }
}

func link(isActive: Binding<Bool>) {}

func test(items: Binding<[Any]>) {
  link(isActive: items[0]) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

struct SendView {
  @Binding var selections: [Bool?]
}

func test(selections: Binding<[Bool]>) {
  _ = SendView(selections[]) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
