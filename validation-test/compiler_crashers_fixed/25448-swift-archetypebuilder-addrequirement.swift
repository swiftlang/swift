// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let:{
class b
protocol a
{
typealias e:b
struct S
enum S<c<typealias e:a
class a
