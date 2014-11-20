// https://devforums.apple.com/thread/254807?tstart=0
// RUN: %swift -emit-ir -verify %s %S/A.swift

class Implementation: A {
    var aValue: Int = 1
}
