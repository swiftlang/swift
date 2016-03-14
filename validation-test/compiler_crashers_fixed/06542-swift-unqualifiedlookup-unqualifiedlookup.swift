// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct Q<H{var b{struct d<T where k=b{}struct d<T where H.g:d
