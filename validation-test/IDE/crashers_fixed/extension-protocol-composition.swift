// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s | %FileCheck %s
// RUN: %target-swift-ide-test -code-completion -code-completion-token=B -source-filename=%s | %FileCheck %s

typealias X = protocol<CustomStringConvertible>
typealias Y = protocol<CustomStringConvertible>
extension protocol<X, Y> {
    func x() { #^A^# }
}
extension AnyObject {
    func x() { #^B^# }
}
// Soundness check results.
// CHECK:  Keyword[let]/None:                  let
