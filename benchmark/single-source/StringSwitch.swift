//===--- StringSwitch.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let benchmarks = [
  BenchmarkInfo(
    name: "StringSwitch",
    runFunction: run_StringSwitch,
    tags: [.validation, .api])
]

@inline(never)
func getIndex(_ s: String) -> Int {
  switch s {
  case "Swift": return 0
  case "is": return 1
  case "a": return 2
  case "general-purpose": return 3
  case "programming language": return 4
  case "built": return 5
  case "using": return 6
  case "modern": return 7
  case "approach": return 8
  case "to": return 9
  case "safety,": return 10
  case "performance,": return 11
  case "and": return 12
  case "software": return 13
  case "design": return 14
  case "patterns.": return 15
  case "": return 16
  case "The": return 17
  case "goal": return 18
  case "of": return 19
  case "the": return 20
  case "project": return 21
  case "create": return 22
  case "best": return 23
  case "available": return 24
  case "for": return 25
  case "uses": return 26
  case "ranging": return 27
  case "from": return 28
  case "systems": return 29
  case "mobile": return 30
  case "desktop": return 31
  case "apps,": return 32
  case "scaling": return 33
  case "up": return 34
  case "cloud": return 35
  case "services.": return 36
  case "Most": return 37
  case "importantly,": return 38
  case "designed": return 39
  case "make": return 40
  case "writing": return 41
  case "maintaining": return 42
  case "correct": return 43
  case "programs": return 44
  case "easier": return 45
  case "developer.": return 46
  case "To": return 47
  case "achieve": return 48
  case "this": return 49
  case "goal,": return 50
  case "we": return 51
  case "believe": return 52
  case "that": return 53
  case "most": return 54
  case "obvious": return 55
  case "way": return 56
  case "write": return 57
  case "code": return 58
  case "must": return 59
  case "also": return 60
  case "be:": return 61
  case "Safe.": return 62
  case "should": return 63
  case "behave": return 64
  case "in": return 65
  case "safe": return 66
  case "manner.": return 67
  case "Undefined": return 68
  case "behavior": return 69
  case "enemy": return 70
  case "developer": return 71
  case "mistakes": return 72
  case "be": return 73
  case "caught": return 74
  case "before": return 75
  case "production.": return 76
  case "Opting": return 77
  case "safety": return 78
  case "sometimes": return 79
  case "means": return 80
  case "will": return 81
  case "feel": return 82
  case "strict,": return 83
  case "but": return 84
  case "clarity": return 85
  case "saves": return 86
  case "time": return 87
  case "long": return 88
  case "run.": return 89
  case "Fast.": return 90
  case "intended": return 91
  case "as": return 92
  case "replacement": return 93
  case "C-based": return 94
  case "languages": return 95
  case "(C, C++, Objective-C).": return 96
  case "As": return 97
  case "such,": return 98
  case "comparable": return 99
  case "those": return 100
  case "performance": return 101
  case "tasks.": return 102
  case "Performance": return 103
  case "predictable": return 104
  case "consistent,": return 105
  case "not": return 106
  case "just": return 107
  case "fast": return 108
  case "short": return 109
  case "bursts": return 110
  case "require": return 111
  case "clean-up": return 112
  case "later.": return 113
  case "There": return 114
  case "are": return 115
  case "lots": return 116
  case "with": return 117
  case "novel": return 118
  case "features": return 119
  case "x": return 120
  case "being": return 121
  case "rare.": return 122
  case "Expressive.": return 123
  case "benefits": return 124
  case "decades": return 125
  case "advancement": return 126
  case "computer": return 127
  case "science": return 128
  case "offer": return 129
  case "syntax": return 130
  case "joy": return 131
  case "use,": return 132
  case "developers": return 133
  case "expect.": return 134
  case "But": return 135
  case "never": return 136
  case "done.": return 137
  case "We": return 138
  case "monitor": return 139
  case "advancements": return 140
  case "embrace": return 141
  case "what": return 142
  case "works,": return 143
  case "continually": return 144
  case "evolving": return 145
  case "even": return 146
  case "better.": return 147
  case "Tools": return 148
  case "critical": return 149
  case "part": return 150
  case "ecosystem.": return 151
  case "strive": return 152
  case "integrate": return 153
  case "well": return 154
  case "within": return 155
  case "developerss": return 156
  case "toolset,": return 157
  case "build": return 158
  case "quickly,": return 159
  case "present": return 160
  case "excellent": return 161
  case "diagnostics,": return 162
  case "enable": return 163
  case "interactive": return 164
  case "development": return 165
  case "experiences.": return 166
  case "can": return 167
  case "so": return 168
  case "much": return 169
  case "more": return 170
  case "powerful,": return 171
  case "like": return 172
  case "Swift-based": return 173
  case "playgrounds": return 174
  case "do": return 175
  case "Xcode,": return 176
  case "or": return 177
  case "web-based": return 178
  case "REPL": return 179
  case "when": return 180
  case "working": return 181
  case "Linux": return 182
  case "server-side": return 183
  case "code.": return 184
  default: return -1
  }
}

@inline(never)
func test(_ s: String) {
  blackHole(getIndex(s))
}

@inline(never)
public func run_StringSwitch(n: Int) {
  let first = "Swift"
  let short = "To"
  let long = "(C, C++, Objective-C)."
  let last = "code."
  let none = "non existent string"
  for _ in 1...100*n {
    test(first)
    test(short)
    test(long)
    test(last)
    test(none)
  }
}

