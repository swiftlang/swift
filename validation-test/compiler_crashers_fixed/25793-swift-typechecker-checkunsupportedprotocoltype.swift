// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol e{protocol c:a{{}}}struct Q{{{}}struct A{enum S{{}enum S{protocol c{func g:S
