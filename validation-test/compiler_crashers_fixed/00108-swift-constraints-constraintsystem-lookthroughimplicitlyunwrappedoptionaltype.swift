// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
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
