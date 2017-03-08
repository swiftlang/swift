// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not --crash %target-swift-frontend %s -emit-ir
{
struct c{}func t(UInt= 1 + 1 + 1 + 1 as?Int){
{{
protocol P{extension{class C:P{
protocol defaulImplementation unon
