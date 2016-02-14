// RUN: %target-swift-frontend -emit-sil %s -import-objc-header %S/Inputs/enum-c.h -verify

func test(value: SwiftEnum) {
    switch value {
    case .One: break
    case .Two: break
    case .Three: break
    } // no error
}
