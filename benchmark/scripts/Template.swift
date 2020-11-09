//===--- {name}.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let {name} = [
  BenchmarkInfo(name: "{name}", runFunction: run_{name}, tags: [.validation, .api]),
]

@inline(never)
public func run_{name}(N: Int) {{
    // TODO
}}
