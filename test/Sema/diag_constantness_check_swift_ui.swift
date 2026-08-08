// RUN: %target-typecheck-verify-swift

// Replicating the SwiftUI framework `ForEach` from the SDK
public struct ForEach<Data, ID, Content> where Data : Swift.RandomAccessCollection, ID: Swift.Hashable {
    public var data: Data
    public var content: (Data.Element) -> Content
}

public protocol View {}
public struct AView: View {}

extension ForEach where Content : View {
    public init(_ data: Data, id: Swift.KeyPath<Data.Element, ID>, content: @escaping (Data.Element) -> Content) {
        self.data = data
        self.content = content
    }
}

extension ForEach where Data == Swift.Range<Swift.Int>, ID == Swift.Int, Content : View {
    @_semantics("swiftui.requires_constant_range") public init(_ data: Swift.Range<Swift.Int>, content: @escaping (Swift.Int) -> Content)
}


func testSwiftUIForEachInvalid() {
    let notReallyConstant = 10
    _ = ForEach(0..<notReallyConstant) { _ in AView() }
    // expected-warning@-1 {{argument must be an integer literal}}
}

func testSwiftUIForEachValid() {
    _ = ForEach(0..<10) { _ in AView() }
    let notReallyConstant = 10
    _ = ForEach(0..<notReallyConstant, id: \.self) { _ in AView() }
}
