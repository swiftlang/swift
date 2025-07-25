//===--- AccessUtils.swift - Utilities for analyzing memory accesses ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// TODO: Move this to AccessUtils.swift when FunctionTest is available.
//
//===----------------------------------------------------------------------===//

import SIL

let getAccessBaseTest = FunctionTest("swift_get_access_base") {
  function, arguments, context in
  let address = arguments.takeValue()
  print("Address: \(address)")
  let base = address.accessBase
  print("Base: \(base)")
}
