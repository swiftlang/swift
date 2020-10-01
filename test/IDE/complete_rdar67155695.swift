// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PUBLIC | %FileCheck %s --check-prefix=PUBLIC
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=INTERNAL | %FileCheck %s --check-prefix=INTERNAL
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE | %FileCheck %s --check-prefix=PRIVATE

public protocol PubP {}

public extension PubP {
    func availableP_availableC() {}

    func availableP_unavailableC() {}

    @available(*, unavailable)
    func unavailableP_availableC() {}

    @available(*, unavailable)
    func unavailableP_unavailableC() {}
}

struct TestForPubP: PubP {
    func availableP_availableC() {}

    @available(*, unavailable)
    func availableP_unavailableC() {}

    func unavailableP_availableC() {}

    @available(*, unavailable)
    func unavailableP_unavailableC() {}
}

func test(val: TestForPubP) {
    val.#^PUBLIC^#
// PUBLIC: Begin completions, 4 items
// PUBLIC-DAG: Keyword[self]/CurrNominal:          self[#TestForPubP#];
// PUBLIC-DAG: Decl[InstanceMethod]/CurrNominal:   unavailableP_availableC()[#Void#];
// PUBLIC-DAG: Decl[InstanceMethod]/Super:         availableP_availableC()[#Void#];
// PUBLIC-DAG: Decl[InstanceMethod]/Super:         availableP_unavailableC()[#Void#];
// PUBLIC: End completions
}

protocol InternalP {}

extension InternalP {
    func availableP_availableC() {}

    func availableP_unavailableC() {}

    @available(*, unavailable)
    func unavailableP_availableC() {}

    @available(*, unavailable)
    func unavailableP_unavailableC() {}
}

struct TestForInternalP: InternalP {
    func availableP_availableC() {}

    @available(*, unavailable)
    func availableP_unavailableC() {}

    func unavailableP_availableC() {}

    @available(*, unavailable)
    func unavailableP_unavailableC() {}
}

func test(val: TestForInternalP) {
    val.#^INTERNAL^#
// INTERNAL: Begin completions, 4 items
// INTERNAL-DAG: Keyword[self]/CurrNominal:          self[#TestForInternalP#];
// INTERNAL-DAG: Decl[InstanceMethod]/CurrNominal:   availableP_availableC()[#Void#];
// INTERNAL-DAG: Decl[InstanceMethod]/CurrNominal:   unavailableP_availableC()[#Void#];
// INTERNAL-DAG: Decl[InstanceMethod]/Super:         availableP_unavailableC()[#Void#];
// INTERNAL: End completions
}

private protocol PrivP {}

private extension PrivP {
    func availableP_availableC() {}

    func availableP_unavailableC() {}

    @available(*, unavailable)
    func unavailableP_availableC() {}

    @available(*, unavailable)
    func unavailableP_unavailableC() {}
}

struct TestForPrivP: PrivP {
    func availableP_availableC() {}

    @available(*, unavailable)
    func availableP_unavailableC() {}

    func unavailableP_availableC() {}

    @available(*, unavailable)
    func unavailableP_unavailableC() {}
}

func test(val: TestForPrivP) {
    val.#^PRIVATE^#
// PRIVATE: Begin completions, 4 items
// PRIVATE-DAG: Keyword[self]/CurrNominal:          self[#TestForPrivP#];
// PRIVATE-DAG: Decl[InstanceMethod]/CurrNominal:   availableP_availableC()[#Void#];
// PRIVATE-DAG: Decl[InstanceMethod]/CurrNominal:   unavailableP_availableC()[#Void#];
// PRIVATE-DAG: Decl[InstanceMethod]/Super:         availableP_unavailableC()[#Void#];
// PRIVATE-DAG: End completions
}
