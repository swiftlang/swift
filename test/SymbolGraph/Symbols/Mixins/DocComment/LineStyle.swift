// ATTN: The RUN lines and associated tests are at the bottom of the file, to
// keep the source locations stable.

/// Single line.
public struct SingleLine {}

/// Two
/// lines.
public struct TwoLines {}

/// Two lines
///
/// Around Blank
public struct TwoLinesAroundBlank {}

///
public struct Empty {}

///
///
///
public struct MultiEmpty {}

///
/// Leading Blank
public struct LeadingBlank {}

/// Trailing Blank
///
public struct TrailingBlank {}

///
/// Bound Blank
///
public struct BoundBlank {}

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name LineStyle -emit-module-path %t/LineStyle.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name LineStyle -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/LineStyle.symbols.json --check-prefix=SINGLELINE
// RUN: %FileCheck %s --input-file %t/LineStyle.symbols.json --check-prefix=TWOLINES
// RUN: %FileCheck %s --input-file %t/LineStyle.symbols.json --check-prefix=TWOLINESAROUNDBLANK
// RUN: %FileCheck %s --input-file %t/LineStyle.symbols.json --check-prefix=EMPTY
// RUN: %FileCheck %s --input-file %t/LineStyle.symbols.json --check-prefix=MULTIEMPTY
// RUN: %FileCheck %s --input-file %t/LineStyle.symbols.json --check-prefix=LEADINGBLANK
// RUN: %FileCheck %s --input-file %t/LineStyle.symbols.json --check-prefix=TRAILINGBLANK

// SINGLELINE-LABEL: "precise": "s:9LineStyle06SingleA0V"
// SINGLELINE:       "docComment": {
// SINGLELINE-NEXT:    "uri": "file://{{.*}}LineStyle.swift",
// SINGLELINE-NEXT:    "module": "LineStyle",
// SINGLELINE-NEXT:    "lines": [
// SINGLELINE-NEXT:      {
// SINGLELINE-NEXT:        "range": {
// SINGLELINE-NEXT:          "start": {
// SINGLELINE-NEXT:            "line": 3,
// SINGLELINE-NEXT:            "character": 4
// SINGLELINE-NEXT:          },
// SINGLELINE-NEXT:          "end": {
// SINGLELINE-NEXT:            "line": 3,
// SINGLELINE-NEXT:            "character": 16
// SINGLELINE-NEXT:          }
// SINGLELINE-NEXT:        },
// SINGLELINE-NEXT:        "text": "Single line."
// SINGLELINE-NEXT:      }
// SINGLELINE-NEXT:    ]
// SINGLELINE-NEXT:  }

// TWOLINES-LABEL: "precise": "s:9LineStyle8TwoLinesV"
// TWOLINES:       "docComment": {
// TWOLINES-NEXT:    "uri": "file://{{.*}}LineStyle.swift",
// TWOLINES-NEXT:    "module": "LineStyle",
// TWOLINES-NEXT:    "lines": [
// TWOLINES-NEXT:      {
// TWOLINES-NEXT:        "range": {
// TWOLINES-NEXT:          "start": {
// TWOLINES-NEXT:            "line": 6,
// TWOLINES-NEXT:            "character": 4
// TWOLINES-NEXT:          },
// TWOLINES-NEXT:          "end": {
// TWOLINES-NEXT:            "line": 6,
// TWOLINES-NEXT:            "character": 7
// TWOLINES-NEXT:          }
// TWOLINES-NEXT:        },
// TWOLINES-NEXT:        "text": "Two"
// TWOLINES-NEXT:      },
// TWOLINES-NEXT:      {
// TWOLINES-NEXT:        "range": {
// TWOLINES-NEXT:          "start": {
// TWOLINES-NEXT:            "line": 7,
// TWOLINES-NEXT:            "character": 4
// TWOLINES-NEXT:          },
// TWOLINES-NEXT:          "end": {
// TWOLINES-NEXT:            "line": 7,
// TWOLINES-NEXT:            "character": 10
// TWOLINES-NEXT:          }
// TWOLINES-NEXT:        },
// TWOLINES-NEXT:        "text": "lines."
// TWOLINES-NEXT:      }
// TWOLINES-NEXT:    ]
// TWOLINES-NEXT:  },

// TWOLINESAROUNDBLANK-LABEL: "precise": "s:9LineStyle19TwoLinesAroundBlankV"
// TWOLINESAROUNDBLANK:       "docComment": {
// TWOLINESAROUNDBLANK-NEXT:    "uri": "file://{{.*}}LineStyle.swift",
// TWOLINESAROUNDBLANK-NEXT:    "module": "LineStyle",
// TWOLINESAROUNDBLANK-NEXT:    "lines": [
// TWOLINESAROUNDBLANK-NEXT:      {
// TWOLINESAROUNDBLANK-NEXT:        "range": {
// TWOLINESAROUNDBLANK-NEXT:          "start": {
// TWOLINESAROUNDBLANK-NEXT:            "line": 10,
// TWOLINESAROUNDBLANK-NEXT:            "character": 4
// TWOLINESAROUNDBLANK-NEXT:          },
// TWOLINESAROUNDBLANK-NEXT:          "end": {
// TWOLINESAROUNDBLANK-NEXT:            "line": 10,
// TWOLINESAROUNDBLANK-NEXT:            "character": 13
// TWOLINESAROUNDBLANK-NEXT:          }
// TWOLINESAROUNDBLANK-NEXT:        },
// TWOLINESAROUNDBLANK-NEXT:        "text": "Two lines"
// TWOLINESAROUNDBLANK-NEXT:      },
// TWOLINESAROUNDBLANK-NEXT:      {
// TWOLINESAROUNDBLANK-NEXT:        "range": {
// TWOLINESAROUNDBLANK-NEXT:          "start": {
// TWOLINESAROUNDBLANK-NEXT:            "line": 11,
// TWOLINESAROUNDBLANK-NEXT:            "character": 3
// TWOLINESAROUNDBLANK-NEXT:          },
// TWOLINESAROUNDBLANK-NEXT:          "end": {
// TWOLINESAROUNDBLANK-NEXT:            "line": 11,
// TWOLINESAROUNDBLANK-NEXT:            "character": 3
// TWOLINESAROUNDBLANK-NEXT:          }
// TWOLINESAROUNDBLANK-NEXT:        },
// TWOLINESAROUNDBLANK-NEXT:        "text": ""
// TWOLINESAROUNDBLANK-NEXT:      },
// TWOLINESAROUNDBLANK-NEXT:      {
// TWOLINESAROUNDBLANK-NEXT:        "range": {
// TWOLINESAROUNDBLANK-NEXT:          "start": {
// TWOLINESAROUNDBLANK-NEXT:            "line": 12,
// TWOLINESAROUNDBLANK-NEXT:            "character": 4
// TWOLINESAROUNDBLANK-NEXT:          },
// TWOLINESAROUNDBLANK-NEXT:          "end": {
// TWOLINESAROUNDBLANK-NEXT:            "line": 12,
// TWOLINESAROUNDBLANK-NEXT:            "character": 16
// TWOLINESAROUNDBLANK-NEXT:          }
// TWOLINESAROUNDBLANK-NEXT:        },
// TWOLINESAROUNDBLANK-NEXT:        "text": "Around Blank"
// TWOLINESAROUNDBLANK-NEXT:      }
// TWOLINESAROUNDBLANK-NEXT:    ]
// TWOLINESAROUNDBLANK-NEXT:  }

// EMPTY-LABEL: "precise": "s:9LineStyle5EmptyV"
// EMPTY:       "docComment": {
// EMPTY-NEXT:    "uri": "file://{{.*}}LineStyle.swift",
// EMPTY-NEXT:    "module": "LineStyle",
// EMPTY-NEXT:    "lines": [
// EMPTY-NEXT:      {
// EMPTY-NEXT:        "range": {
// EMPTY-NEXT:          "start": {
// EMPTY-NEXT:            "line": 15,
// EMPTY-NEXT:            "character": 3
// EMPTY-NEXT:          },
// EMPTY-NEXT:          "end": {
// EMPTY-NEXT:            "line": 15,
// EMPTY-NEXT:            "character": 3
// EMPTY-NEXT:          }
// EMPTY-NEXT:        },
// EMPTY-NEXT:        "text": ""
// EMPTY-NEXT:      }
// EMPTY-NEXT:    ]
// EMPTY-NEXT:  },

// MULTIEMPTY-LABEL: "precise": "s:9LineStyle10MultiEmptyV"
// MULTIEMPTY:       "docComment": {
// MULTIEMPTY-NEXT:    "uri": "file://{{.*}}LineStyle.swift",
// MULTIEMPTY-NEXT:    "module": "LineStyle",
// MULTIEMPTY-NEXT:    "lines": [
// MULTIEMPTY-NEXT:      {
// MULTIEMPTY-NEXT:        "range": {
// MULTIEMPTY-NEXT:          "start": {
// MULTIEMPTY-NEXT:            "line": 18,
// MULTIEMPTY-NEXT:            "character": 3
// MULTIEMPTY-NEXT:          },
// MULTIEMPTY-NEXT:          "end": {
// MULTIEMPTY-NEXT:            "line": 18,
// MULTIEMPTY-NEXT:            "character": 3
// MULTIEMPTY-NEXT:          }
// MULTIEMPTY-NEXT:        },
// MULTIEMPTY-NEXT:        "text": ""
// MULTIEMPTY-NEXT:      },
// MULTIEMPTY-NEXT:      {
// MULTIEMPTY-NEXT:        "range": {
// MULTIEMPTY-NEXT:          "start": {
// MULTIEMPTY-NEXT:            "line": 19,
// MULTIEMPTY-NEXT:            "character": 3
// MULTIEMPTY-NEXT:          },
// MULTIEMPTY-NEXT:          "end": {
// MULTIEMPTY-NEXT:            "line": 19,
// MULTIEMPTY-NEXT:            "character": 3
// MULTIEMPTY-NEXT:          }
// MULTIEMPTY-NEXT:        },
// MULTIEMPTY-NEXT:        "text": ""
// MULTIEMPTY-NEXT:      },
// MULTIEMPTY-NEXT:      {
// MULTIEMPTY-NEXT:        "range": {
// MULTIEMPTY-NEXT:          "start": {
// MULTIEMPTY-NEXT:            "line": 20,
// MULTIEMPTY-NEXT:            "character": 3
// MULTIEMPTY-NEXT:          },
// MULTIEMPTY-NEXT:          "end": {
// MULTIEMPTY-NEXT:            "line": 20,
// MULTIEMPTY-NEXT:            "character": 3
// MULTIEMPTY-NEXT:          }
// MULTIEMPTY-NEXT:        },
// MULTIEMPTY-NEXT:        "text": ""
// MULTIEMPTY-NEXT:      }
// MULTIEMPTY-NEXT:    ]
// MULTIEMPTY-NEXT:  },

// LEADINGBLANK-LABEL: "precise": "s:9LineStyle12LeadingBlankV",
// LEADINGBLANK:       "docComment": {
// LEADINGBLANK-NEXT:    "uri": "file://{{.*}}LineStyle.swift",
// LEADINGBLANK-NEXT:    "module": "LineStyle",
// LEADINGBLANK-NEXT:    "lines": [
// LEADINGBLANK-NEXT:      {
// LEADINGBLANK-NEXT:        "range": {
// LEADINGBLANK-NEXT:          "start": {
// LEADINGBLANK-NEXT:            "line": 23,
// LEADINGBLANK-NEXT:            "character": 3
// LEADINGBLANK-NEXT:          },
// LEADINGBLANK-NEXT:          "end": {
// LEADINGBLANK-NEXT:            "line": 23,
// LEADINGBLANK-NEXT:            "character": 3
// LEADINGBLANK-NEXT:          }
// LEADINGBLANK-NEXT:        },
// LEADINGBLANK-NEXT:        "text": ""
// LEADINGBLANK-NEXT:      },
// LEADINGBLANK-NEXT:      {
// LEADINGBLANK-NEXT:        "range": {
// LEADINGBLANK-NEXT:          "start": {
// LEADINGBLANK-NEXT:            "line": 24,
// LEADINGBLANK-NEXT:            "character": 4
// LEADINGBLANK-NEXT:          },
// LEADINGBLANK-NEXT:          "end": {
// LEADINGBLANK-NEXT:            "line": 24,
// LEADINGBLANK-NEXT:            "character": 17
// LEADINGBLANK-NEXT:          }
// LEADINGBLANK-NEXT:        },
// LEADINGBLANK-NEXT:        "text": "Leading Blank"
// LEADINGBLANK-NEXT:      }
// LEADINGBLANK-NEXT:    ]
// LEADINGBLANK-NEXT:  }

// TRAILINGBLANK-LABEL: "precise": "s:9LineStyle13TrailingBlankV"
// TRAILINGBLANK:       "docComment": {
// TRAILINGBLANK-NEXT:    "uri": "file://{{.*}}LineStyle.swift",
// TRAILINGBLANK-NEXT:    "module": "LineStyle",
// TRAILINGBLANK-NEXT:    "lines": [
// TRAILINGBLANK-NEXT:      {
// TRAILINGBLANK-NEXT:        "range": {
// TRAILINGBLANK-NEXT:          "start": {
// TRAILINGBLANK-NEXT:            "line": 27,
// TRAILINGBLANK-NEXT:            "character": 4
// TRAILINGBLANK-NEXT:          },
// TRAILINGBLANK-NEXT:          "end": {
// TRAILINGBLANK-NEXT:            "line": 27,
// TRAILINGBLANK-NEXT:            "character": 18
// TRAILINGBLANK-NEXT:          }
// TRAILINGBLANK-NEXT:        },
// TRAILINGBLANK-NEXT:        "text": "Trailing Blank"
// TRAILINGBLANK-NEXT:      },
// TRAILINGBLANK-NEXT:      {
// TRAILINGBLANK-NEXT:        "range": {
// TRAILINGBLANK-NEXT:          "start": {
// TRAILINGBLANK-NEXT:            "line": 28,
// TRAILINGBLANK-NEXT:            "character": 3
// TRAILINGBLANK-NEXT:          },
// TRAILINGBLANK-NEXT:          "end": {
// TRAILINGBLANK-NEXT:            "line": 28,
// TRAILINGBLANK-NEXT:            "character": 3
// TRAILINGBLANK-NEXT:          }
// TRAILINGBLANK-NEXT:        },
// TRAILINGBLANK-NEXT:        "text": ""
// TRAILINGBLANK-NEXT:      }
// TRAILINGBLANK-NEXT:    ]
// TRAILINGBLANK-NEXT:  },

// BOUNDBLANK-LABEL: "precise": "s:9LineStyle10BoundBlankV"
// BOUNDBLANK:       "docComment": {
// BOUNDBLANK-NEXT:    "uri": "file://{{.*}}LineStyle.swift",
// BOUNDBLANK-NEXT:    "module": "LineStyle",
// BOUNDBLANK-NEXT:    "lines": [
// BOUNDBLANK-NEXT:      {
// BOUNDBLANK-NEXT:        "range": {
// BOUNDBLANK-NEXT:          "start": {
// BOUNDBLANK-NEXT:            "line": 31,
// BOUNDBLANK-NEXT:            "character": 3
// BOUNDBLANK-NEXT:          },
// BOUNDBLANK-NEXT:          "end": {
// BOUNDBLANK-NEXT:            "line": 31,
// BOUNDBLANK-NEXT:            "character": 3
// BOUNDBLANK-NEXT:          }
// BOUNDBLANK-NEXT:        },
// BOUNDBLANK-NEXT:        "text": ""
// BOUNDBLANK-NEXT:      },
// BOUNDBLANK-NEXT:      {
// BOUNDBLANK-NEXT:        "range": {
// BOUNDBLANK-NEXT:          "start": {
// BOUNDBLANK-NEXT:            "line": 32,
// BOUNDBLANK-NEXT:            "character": 3
// BOUNDBLANK-NEXT:          },
// BOUNDBLANK-NEXT:          "end": {
// BOUNDBLANK-NEXT:            "line": 32,
// BOUNDBLANK-NEXT:            "character": 15
// BOUNDBLANK-NEXT:          }
// BOUNDBLANK-NEXT:        },
// BOUNDBLANK-NEXT:        "text": " Bound Blank"
// BOUNDBLANK-NEXT:      },
// BOUNDBLANK-NEXT:      {
// BOUNDBLANK-NEXT:        "range": {
// BOUNDBLANK-NEXT:          "start": {
// BOUNDBLANK-NEXT:            "line": 33,
// BOUNDBLANK-NEXT:            "character": 3
// BOUNDBLANK-NEXT:          },
// BOUNDBLANK-NEXT:          "end": {
// BOUNDBLANK-NEXT:            "line": 33,
// BOUNDBLANK-NEXT:            "character": 3
// BOUNDBLANK-NEXT:          }
// BOUNDBLANK-NEXT:        },
// BOUNDBLANK-NEXT:        "text": ""
// BOUNDBLANK-NEXT:      }
// BOUNDBLANK-NEXT:    ]
// BOUNDBLANK-NEXT:  },
