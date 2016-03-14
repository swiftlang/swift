// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{struct Q<I{struct A:OptionSetType{let rawValue=""let rawValue=0
