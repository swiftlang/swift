// RUN: %target-swift-frontend -emit-ir -primary-file %s %S/Inputs/issue-51714-other.swift -module-name M
// RUN: %target-swift-frontend -emit-ir %s -primary-file %S/Inputs/issue-51714-other.swift -module-name M

// https://github.com/apple/swift/issues/51714

protocol P1 {
    associatedtype P1_T1
    associatedtype P1_T2: Collection where P1_T2.Element == P1_T1
}

protocol P2 {
    associatedtype P2_T
}

protocol P3: P1, P2 {
    associatedtype P3_T: P1 where P3_T.P1_T2 == Range<P2_T>
}
