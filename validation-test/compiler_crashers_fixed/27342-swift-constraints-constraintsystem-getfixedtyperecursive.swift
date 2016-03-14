// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct Q<T{
struct B{
let b=0
var a{{
struct Q<l:T.c
