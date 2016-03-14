// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{struct B:a{{}}B<f{}class a{enum a{struct i{class a{struct c<T:T.e
