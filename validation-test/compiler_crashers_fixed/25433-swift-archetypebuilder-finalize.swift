// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol A{
func c:A
protocol A{typealias h:B
class B{
struct Q<d where h:A
