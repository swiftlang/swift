// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
extension{protocol a{class a{protocol C{func<}}func<}}{<T
