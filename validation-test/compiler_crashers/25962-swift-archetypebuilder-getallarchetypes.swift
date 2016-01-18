// RUN: not --crash %target-swift-frontend %s -parse
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/adocyn (adocyn)

extension CollectionType {
    func f<T where T=Generator>() {
    }
}
