// RUN: not %target-swift-frontend -typecheck %s -solver-scope-threshold=100000 -diagnostic-style llvm 2>&1 | %FileCheck %s
// REQUIRES: OS=macosx

import Foundation
import RegexBuilder

// CHECK: rdar119215676.swift:{{.*}}: error: the compiler is unable to type-check this expression in reasonable time
internal enum DODRelativeDateRegexes {
    static let year = Reference<Int?>()
    static let month = Reference<Int?>()
    static let day = Reference<Int?>()
    static let hour = Reference<Int?>()
    static let minute = Reference<Int?>()
    static let second = Reference<Int?>()
    static let nanosecond = Reference<Int?>()

    static let yearRegex = Repeat(0...1) {
        Capture(as: year) {
            Regex {
                Repeat(1...2) {
                    One(.digit)
                }
                "y"
            }
        } transform: { rawInput in
            return Int(rawInput.lowercased().replacingOccurrences(of: "y", with: ""))
        }
    }

    static let monthRegex = Repeat(0...1) {
        Capture(as: month) {
            Regex {
                Capture {
                    ChoiceOf {
                        Regex {
                            "1"
                            ("0"..."2")
                        }
                        ("1"..."9")
                    }
                }
                "m"
            }
        } transform: { rawInput in
            return Int(rawInput.lowercased().replacingOccurrences(of: "m", with: ""))
        }
    }

    static let dayRegex = Repeat(0...1) {
        Capture(as: day) {
            Regex {
                OneOrMore(.digit)
                "d"
            }
        } transform: { rawInput in
            return Int(rawInput.lowercased().replacingOccurrences(of: "d", with: ""))
        }
    }

    static let hourRegex = ZeroOrMore {
        Capture(as: hour) {
            Regex {
                "@"
                Capture {
                    ChoiceOf {  // expected-error {{reasonable time}}
                        Regex {
                            Repeat(0...1) {
                                "0"
                            }
                            ("0"..."9")
                        }
                        Regex {
                            "1"
                            ("0"..."9")
                        }
                        Regex {
                            "2"
                            ("0"..."4")
                        }
                    }
                }
            }
        } transform: { rawInput in
            return Int(rawInput.replacingOccurrences(of: "@", with: ""))
        }
    }

    internal static let sixtyRangeCapture = Regex {
        ":"
        Capture {
            ChoiceOf {
                Regex {
                    "0"
                    ("0"..."9")
                }
                Regex {
                    ("1"..."5")
                    ("0"..."9")
                }
            }
        }
    }

    static let minuteRegex = Repeat(0...1) {
        Capture(as: minute) {
            sixtyRangeCapture
        } transform: { rawInput in
            return Int(rawInput.replacingOccurrences(of: ":", with: ""))
        }
    }

    static let secondRegex = Repeat(0...1) {
        Capture(as: second) {
            sixtyRangeCapture
        } transform: { rawInput in
            return Int(rawInput.replacingOccurrences(of: ":", with: ""))
        }
    }

    static let nanosecondRegex = ZeroOrMore {
        Capture(as: nanosecond) {
            Regex {
                "."
                Repeat(count: 3) {
                    One(.digit)
                }
            }
        } transform: { rawInput in
            return Int(rawInput.replacingOccurrences(of: ".", with: ""))
        }
    }
}
