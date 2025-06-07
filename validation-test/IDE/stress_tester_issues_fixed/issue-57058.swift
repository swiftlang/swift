// RUN: %target-swift-ide-test --code-completion --source-filename %s --code-completion-token=COMPLETE | %FileCheck %s

// https://github.com/apple/swift/issues/57058

protocol View2 {}
 
extension Never : View2 {}

struct Text2: View2 {}

@resultBuilder struct ViewBuilder2 {
	static func buildBlock() -> Never { fatalError() }
	static func buildBlock<Content>(_ content: Content) -> Content where Content : View2 { fatalError() }
}

struct MysteryIsland2 {
    let chance: Int = 2
}

struct VStack2<Content> : View2 where Content : View2 {
    init(@ViewBuilder2 content: () -> Content) { fatalError() }
}

struct MysteryIslandDetail {
    let island: MysteryIsland2
    
    @ViewBuilder2 var body: some View2 {
        let b = "\(island.#^COMPLETE^#chance)"
        VStack2() {}
    }
}

// CHECK: Begin completions, 2 items
// CHECK-DAG: Keyword[self]/CurrNominal:          self[#MysteryIsland2#]; name=self
// CHECK-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]: chance[#Int#]; name=chance
