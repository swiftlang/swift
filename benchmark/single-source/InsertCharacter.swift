//===--- InsertCharacter.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let InsertCharacter = [
    BenchmarkInfo(name: "InsertCharacterEndIndex", runFunction: run_InsertCharacterEndIndex, tags: [.validation, .api], setUpFunction: buildWorkload),
    BenchmarkInfo(name: "InsertCharacterTowardsEndIndex", runFunction: run_InsertCharacterTowardsEndIndex, tags: [.validation, .api], setUpFunction: buildWorkload),
    BenchmarkInfo(name: "InsertCharacterStartIndex", runFunction: run_InsertCharacterStartIndex, tags: [.validation, .api], setUpFunction: buildWorkload),
    BenchmarkInfo(name: "InsertCharacterEndIndexNonASCII", runFunction: run_InsertCharacterEndIndexNonASCII, tags: [.validation, .api], setUpFunction: buildWorkload),
    BenchmarkInfo(name: "InsertCharacterTowardsEndIndexNonASCII", runFunction: run_InsertCharacterTowardsEndIndexNonASCII, tags: [.validation, .api], setUpFunction: buildWorkload),
    BenchmarkInfo(name: "InsertCharacterStartIndexNonASCII", runFunction: run_InsertCharacterStartIndexNonASCII, tags: [.validation, .api], setUpFunction: buildWorkload)
]

let str = String(repeating: "A very long ASCII string.", count: 200)

func buildWorkload() {
    blackHole(str)
}

@inline(never)
func run_InsertCharacterEndIndex(_ N: Int) {
    var workload = str
    let character: Character = "a"
    let scale = 3000
    for _ in 0..<N * scale {
        workload.insert(character, at: workload.endIndex)
    }
    blackHole(workload)
    CheckResults(workload.count == str.count + N * scale)
}

@inline(never)
func run_InsertCharacterTowardsEndIndex(_ N: Int) {
    var workload = str
    let character: Character = "b"
    var index = workload.endIndex
    let scale = 3000
    for i in 0..<N * scale {
        workload.insert(character, at: index)
        if i % 1000 == 0 {
            index = workload.endIndex
        }
    }
    blackHole(workload)
    CheckResults(workload.count == str.count + N * scale)
}

@inline(never)
func run_InsertCharacterStartIndex(_ N: Int) {
    var workload = str
    let character: Character = "c"
    let scale = 75
    let insertionsCount = 50
    for _ in 0..<N * scale {
        for _ in 0..<insertionsCount {
            workload.insert(character, at: workload.startIndex)
        }
        workload = str
    }
    blackHole(workload)
    CheckResults(workload.count == str.count)
}

@inline(never)
func run_InsertCharacterEndIndexNonASCII(_ N: Int) {
    var workload = str
    let character: Character = "👩🏼‍💻"
    let scale = 1000
    for _ in 0..<N * scale {
        workload.insert(character, at: workload.endIndex)
    }
    blackHole(workload)
    CheckResults(workload.count == str.count + N * scale)
}

@inline(never)
func run_InsertCharacterTowardsEndIndexNonASCII(_ N: Int) {
    var workload = str
    let character: Character = "👩🏾‍🏫"
    var index = workload.endIndex
    let scale = 1000
    for i in 0..<N * scale {
        workload.insert(character, at: index)
        if i % 100 == 0 {
            index = workload.endIndex
        }
    }
    blackHole(workload)
    CheckResults(workload.count == str.count + N * scale)
}

@inline(never)
func run_InsertCharacterStartIndexNonASCII(_ N: Int) {
    var workload = str
    let character: Character = "👨🏽‍🏫"
    let scale = 50
    for _ in 0..<N * scale {
        for _ in 0..<25 {
            workload.insert(character, at: workload.startIndex)
        }
        workload = str
    }
    blackHole(workload)
    CheckResults(workload.count == str.count)
}
