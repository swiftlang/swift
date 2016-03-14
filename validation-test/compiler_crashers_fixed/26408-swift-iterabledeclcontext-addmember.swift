// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol A{class B<T:a}extension String{struct Q{struct d{struct c{class B
