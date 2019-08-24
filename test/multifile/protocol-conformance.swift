// RUN: %target-swift-frontend -emit-ir -verify %s %S/Inputs/protocol-conformance/A.swift

// https://devforums.apple.com/thread/254807?tstart=0

class Implementation: A {
    var aValue: Int = 1
}
