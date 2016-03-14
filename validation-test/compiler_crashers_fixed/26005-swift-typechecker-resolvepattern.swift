// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol C{protocol e:S}struct T{struct A{struct Q{struct A{enum a{let:
