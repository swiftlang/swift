// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -enable-experimental-cxx-interop

import ConformsTo

protocol Testable {
    func test()
}

protocol Playable {
    func play()
}

func callee(_ _: Testable) {

}

func caller(_ x: HasTest) {
    callee(x)
}

func callee(_ _: Playable) {

}

func caller(_ x: Playable) {
    callee(x)
}
