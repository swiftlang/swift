// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func<{struct B<T where B=o{var:BooleanType{var:T.h={
