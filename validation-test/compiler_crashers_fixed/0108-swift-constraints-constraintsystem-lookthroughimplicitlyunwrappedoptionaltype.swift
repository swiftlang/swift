// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

d = i
}
class d<j : i, f : i where j.i == f> : e {
}
class d<j, f> {
}
protocol i {
    typealias i
}
protocol e {
    class func i()
}
i
(d() as e).j.i()
d
protocol i : d { func d
