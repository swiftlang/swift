// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{struct Q{class B:Collection}class B<T{struct Q<T where T:B{class A:B<T>
