// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

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
