// RUN: %target-swift-frontend -emit-sil %s -import-objc-header %S/Inputs/enum-objc.h -verify

// REQUIRES: objc_interop

func test(_ value: SwiftEnum) {
    switch value {
    case .one: break
    case .two: break
    case .three: break
    } // no error
}
