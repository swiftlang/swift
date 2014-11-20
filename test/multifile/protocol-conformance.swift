// https://devforums.apple.com/thread/254807?tstart=0
// RUN: %swift -emit-ir -verify %s %S/Inputs/protocol-conformance/A.swift

class Implementation: A {
    var aValue: Int = 1
}
