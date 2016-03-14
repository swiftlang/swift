// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{struct Q{class a{struct e{struct A{class A{{}protocol a}}}}}struct Q<T:T.t
