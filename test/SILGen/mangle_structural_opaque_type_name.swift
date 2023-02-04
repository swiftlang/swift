// RUN: %target-swift-emit-silgen -disable-availability-checking -verify %s

protocol View { }

extension String: View {}

struct Button<V: View>: View {
    init(_: V) {}
}

extension View {
    func buttonStyle<S>(_: S) -> some View { return self }
}

struct VStack<V: View>: View {
    init(_: () -> V) {}
}

struct TupleView<V: View, W: View>: View {
    init(_: V, _: W) {}
}

struct ContentView: View {
    var body: some View {
        VStack { TupleView(sampleButton1.buttonStyle(17),
                           sampleButton2.buttonStyle(38)) }
    }

    var sampleButton1: Button<some View> {
        Button("Tap Here")
    }
    
    var sampleButton2: Button<some View> {
        Button("And Here")
    }
}
