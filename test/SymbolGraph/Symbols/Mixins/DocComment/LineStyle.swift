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
// SINGLELINE-NEXT:    "lines": [
// SINGLELINE-NEXT:      {
// SINGLELINE-NEXT:        "range": {
// SINGLELINE-NEXT:          "start": {
// SINGLELINE-NEXT:            "line": 30,
// SINGLELINE-NEXT:            "character": 4
// SINGLELINE-NEXT:          },
// SINGLELINE-NEXT:          "end": {
// SINGLELINE-NEXT:            "line": 30,
// SINGLELINE-NEXT:            "character": 16
// SINGLELINE-NEXT:          }
// SINGLELINE-NEXT:        },
// SINGLELINE-NEXT:        "text": "Single line."
// SINGLELINE-NEXT:      }
// SINGLELINE-NEXT:    ]
// SINGLELINE-NEXT:  }

/// Single line.
public struct SingleLine {}

// TWOLINES-LABEL: "precise": "s:9LineStyle8TwoLinesV"
// TWOLINES:       "docComment": {
// TWOLINES-NEXT:    "lines": [
// TWOLINES-NEXT:      {
// TWOLINES-NEXT:        "range": {
// TWOLINES-NEXT:          "start": {
// TWOLINES-NEXT:            "line": 65,
// TWOLINES-NEXT:            "character": 4
// TWOLINES-NEXT:          },
// TWOLINES-NEXT:          "end": {
// TWOLINES-NEXT:            "line": 65,
// TWOLINES-NEXT:            "character": 7
// TWOLINES-NEXT:          }
// TWOLINES-NEXT:        },
// TWOLINES-NEXT:        "text": "Two"
// TWOLINES-NEXT:      },
// TWOLINES-NEXT:      {
// TWOLINES-NEXT:        "range": {
// TWOLINES-NEXT:          "start": {
// TWOLINES-NEXT:            "line": 66,
// TWOLINES-NEXT:            "character": 4
// TWOLINES-NEXT:          },
// TWOLINES-NEXT:          "end": {
// TWOLINES-NEXT:            "line": 66,
// TWOLINES-NEXT:            "character": 10
// TWOLINES-NEXT:          }
// TWOLINES-NEXT:        },
// TWOLINES-NEXT:        "text": "lines."
// TWOLINES-NEXT:      }
// TWOLINES-NEXT:    ]
// TWOLINES-NEXT:  },

/// Two
/// lines.
public struct TwoLines {}

// TWOLINESAROUNDBLANK-LABEL: "precise": "s:9LineStyle19TwoLinesAroundBlankV"
// TWOLINESAROUNDBLANK:       "docComment": {
// TWOLINESAROUNDBLANK-NEXT:    "lines": [
// TWOLINESAROUNDBLANK-NEXT:      {
// TWOLINESAROUNDBLANK-NEXT:        "range": {
// TWOLINESAROUNDBLANK-NEXT:          "start": {
// TWOLINESAROUNDBLANK-NEXT:            "line": 114,
// TWOLINESAROUNDBLANK-NEXT:            "character": 4
// TWOLINESAROUNDBLANK-NEXT:          },
// TWOLINESAROUNDBLANK-NEXT:          "end": {
// TWOLINESAROUNDBLANK-NEXT:            "line": 114,
// TWOLINESAROUNDBLANK-NEXT:            "character": 13
// TWOLINESAROUNDBLANK-NEXT:          }
// TWOLINESAROUNDBLANK-NEXT:        },
// TWOLINESAROUNDBLANK-NEXT:        "text": "Two lines"
// TWOLINESAROUNDBLANK-NEXT:      },
// TWOLINESAROUNDBLANK-NEXT:      {
// TWOLINESAROUNDBLANK-NEXT:        "range": {
// TWOLINESAROUNDBLANK-NEXT:          "start": {
// TWOLINESAROUNDBLANK-NEXT:            "line": 115,
// TWOLINESAROUNDBLANK-NEXT:            "character": 3
// TWOLINESAROUNDBLANK-NEXT:          },
// TWOLINESAROUNDBLANK-NEXT:          "end": {
// TWOLINESAROUNDBLANK-NEXT:            "line": 115,
// TWOLINESAROUNDBLANK-NEXT:            "character": 3
// TWOLINESAROUNDBLANK-NEXT:          }
// TWOLINESAROUNDBLANK-NEXT:        },
// TWOLINESAROUNDBLANK-NEXT:        "text": ""
// TWOLINESAROUNDBLANK-NEXT:      },
// TWOLINESAROUNDBLANK-NEXT:      {
// TWOLINESAROUNDBLANK-NEXT:        "range": {
// TWOLINESAROUNDBLANK-NEXT:          "start": {
// TWOLINESAROUNDBLANK-NEXT:            "line": 116,
// TWOLINESAROUNDBLANK-NEXT:            "character": 4
// TWOLINESAROUNDBLANK-NEXT:          },
// TWOLINESAROUNDBLANK-NEXT:          "end": {
// TWOLINESAROUNDBLANK-NEXT:            "line": 116,
// TWOLINESAROUNDBLANK-NEXT:            "character": 16
// TWOLINESAROUNDBLANK-NEXT:          }
// TWOLINESAROUNDBLANK-NEXT:        },
// TWOLINESAROUNDBLANK-NEXT:        "text": "Around Blank"
// TWOLINESAROUNDBLANK-NEXT:      }
// TWOLINESAROUNDBLANK-NEXT:    ]
// TWOLINESAROUNDBLANK-NEXT:  }

/// Two lines
///
/// Around Blank
public struct TwoLinesAroundBlank {}

// EMPTY-LABEL: "precise": "s:9LineStyle5EmptyV"
// EMPTY:       "docComment": {
// EMPTY-NEXT:    "lines": [
// EMPTY-NEXT:      {
// EMPTY-NEXT:        "range": {
// EMPTY-NEXT:          "start": {
// EMPTY-NEXT:            "line": 138,
// EMPTY-NEXT:            "character": 3
// EMPTY-NEXT:          },
// EMPTY-NEXT:          "end": {
// EMPTY-NEXT:            "line": 138,
// EMPTY-NEXT:            "character": 3
// EMPTY-NEXT:          }
// EMPTY-NEXT:        },
// EMPTY-NEXT:        "text": ""
// EMPTY-NEXT:      }
// EMPTY-NEXT:    ]
// EMPTY-NEXT:  },

///
public struct Empty {}

// MULTIEMPTY-LABEL: "precise": "s:9LineStyle10MultiEmptyV"
// MULTIEMPTY:       "docComment": {
// MULTIEMPTY-NEXT:    "lines": [
// MULTIEMPTY-NEXT:      {
// MULTIEMPTY-NEXT:        "range": {
// MULTIEMPTY-NEXT:          "start": {
// MULTIEMPTY-NEXT:            "line": 186,
// MULTIEMPTY-NEXT:            "character": 3
// MULTIEMPTY-NEXT:          },
// MULTIEMPTY-NEXT:          "end": {
// MULTIEMPTY-NEXT:            "line": 186,
// MULTIEMPTY-NEXT:            "character": 3
// MULTIEMPTY-NEXT:          }
// MULTIEMPTY-NEXT:        },
// MULTIEMPTY-NEXT:        "text": ""
// MULTIEMPTY-NEXT:      },
// MULTIEMPTY-NEXT:      {
// MULTIEMPTY-NEXT:        "range": {
// MULTIEMPTY-NEXT:          "start": {
// MULTIEMPTY-NEXT:            "line": 187,
// MULTIEMPTY-NEXT:            "character": 3
// MULTIEMPTY-NEXT:          },
// MULTIEMPTY-NEXT:          "end": {
// MULTIEMPTY-NEXT:            "line": 187,
// MULTIEMPTY-NEXT:            "character": 3
// MULTIEMPTY-NEXT:          }
// MULTIEMPTY-NEXT:        },
// MULTIEMPTY-NEXT:        "text": ""
// MULTIEMPTY-NEXT:      },
// MULTIEMPTY-NEXT:      {
// MULTIEMPTY-NEXT:        "range": {
// MULTIEMPTY-NEXT:          "start": {
// MULTIEMPTY-NEXT:            "line": 188,
// MULTIEMPTY-NEXT:            "character": 3
// MULTIEMPTY-NEXT:          },
// MULTIEMPTY-NEXT:          "end": {
// MULTIEMPTY-NEXT:            "line": 188,
// MULTIEMPTY-NEXT:            "character": 3
// MULTIEMPTY-NEXT:          }
// MULTIEMPTY-NEXT:        },
// MULTIEMPTY-NEXT:        "text": ""
// MULTIEMPTY-NEXT:      }
// MULTIEMPTY-NEXT:    ]
// MULTIEMPTY-NEXT:  },

///
///
///
public struct MultiEmpty {}

// LEADINGBLANK-LABEL: "precise": "s:9LineStyle12LeadingBlankV",
// LEADINGBLANK:       "docComment": {
// LEADINGBLANK-NEXT:    "lines": [
// LEADINGBLANK-NEXT:      {
// LEADINGBLANK-NEXT:        "range": {
// LEADINGBLANK-NEXT:          "start": {
// LEADINGBLANK-NEXT:            "line": 223,
// LEADINGBLANK-NEXT:            "character": 3
// LEADINGBLANK-NEXT:          },
// LEADINGBLANK-NEXT:          "end": {
// LEADINGBLANK-NEXT:            "line": 223,
// LEADINGBLANK-NEXT:            "character": 3
// LEADINGBLANK-NEXT:          }
// LEADINGBLANK-NEXT:        },
// LEADINGBLANK-NEXT:        "text": ""
// LEADINGBLANK-NEXT:      },
// LEADINGBLANK-NEXT:      {
// LEADINGBLANK-NEXT:        "range": {
// LEADINGBLANK-NEXT:          "start": {
// LEADINGBLANK-NEXT:            "line": 224,
// LEADINGBLANK-NEXT:            "character": 4
// LEADINGBLANK-NEXT:          },
// LEADINGBLANK-NEXT:          "end": {
// LEADINGBLANK-NEXT:            "line": 224,
// LEADINGBLANK-NEXT:            "character": 17
// LEADINGBLANK-NEXT:          }
// LEADINGBLANK-NEXT:        },
// LEADINGBLANK-NEXT:        "text": "Leading Blank"
// LEADINGBLANK-NEXT:      }
// LEADINGBLANK-NEXT:    ]
// LEADINGBLANK-NEXT:  }

///
/// Leading Blank
public struct LeadingBlank {}

// TRAILINGBLANK-LABEL: "precise": "s:9LineStyle13TrailingBlankV"
// TRAILINGBLANK:       "docComment": {
// TRAILINGBLANK-NEXT:    "lines": [
// TRAILINGBLANK-NEXT:      {
// TRAILINGBLANK-NEXT:        "range": {
// TRAILINGBLANK-NEXT:          "start": {
// TRAILINGBLANK-NEXT:            "line": 259,
// TRAILINGBLANK-NEXT:            "character": 4
// TRAILINGBLANK-NEXT:          },
// TRAILINGBLANK-NEXT:          "end": {
// TRAILINGBLANK-NEXT:            "line": 259,
// TRAILINGBLANK-NEXT:            "character": 18
// TRAILINGBLANK-NEXT:          }
// TRAILINGBLANK-NEXT:        },
// TRAILINGBLANK-NEXT:        "text": "Trailing Blank"
// TRAILINGBLANK-NEXT:      },
// TRAILINGBLANK-NEXT:      {
// TRAILINGBLANK-NEXT:        "range": {
// TRAILINGBLANK-NEXT:          "start": {
// TRAILINGBLANK-NEXT:            "line": 260,
// TRAILINGBLANK-NEXT:            "character": 3
// TRAILINGBLANK-NEXT:          },
// TRAILINGBLANK-NEXT:          "end": {
// TRAILINGBLANK-NEXT:            "line": 260,
// TRAILINGBLANK-NEXT:            "character": 3
// TRAILINGBLANK-NEXT:          }
// TRAILINGBLANK-NEXT:        },
// TRAILINGBLANK-NEXT:        "text": ""
// TRAILINGBLANK-NEXT:      }
// TRAILINGBLANK-NEXT:    ]
// TRAILINGBLANK-NEXT:  },

/// Trailing Blank
///
public struct TrailingBlank {} 

// BOUNDBLANK-LABEL: "precise": "s:9LineStyle10BoundBlankV"
// BOUNDBLANK:       "docComment": {
// BOUNDBLANK-NEXT:    "lines": [
// BOUNDBLANK-NEXT:      {
// BOUNDBLANK-NEXT:        "range": {
// BOUNDBLANK-NEXT:          "start": {
// BOUNDBLANK-NEXT:            "line": 308,
// BOUNDBLANK-NEXT:            "character": 3
// BOUNDBLANK-NEXT:          },
// BOUNDBLANK-NEXT:          "end": {
// BOUNDBLANK-NEXT:            "line": 308,
// BOUNDBLANK-NEXT:            "character": 3
// BOUNDBLANK-NEXT:          }
// BOUNDBLANK-NEXT:        },
// BOUNDBLANK-NEXT:        "text": ""
// BOUNDBLANK-NEXT:      },
// BOUNDBLANK-NEXT:      {
// BOUNDBLANK-NEXT:        "range": {
// BOUNDBLANK-NEXT:          "start": {
// BOUNDBLANK-NEXT:            "line": 309,
// BOUNDBLANK-NEXT:            "character": 3
// BOUNDBLANK-NEXT:          },
// BOUNDBLANK-NEXT:          "end": {
// BOUNDBLANK-NEXT:            "line": 309,
// BOUNDBLANK-NEXT:            "character": 15
// BOUNDBLANK-NEXT:          }
// BOUNDBLANK-NEXT:        },
// BOUNDBLANK-NEXT:        "text": " Bound Blank"
// BOUNDBLANK-NEXT:      },
// BOUNDBLANK-NEXT:      {
// BOUNDBLANK-NEXT:        "range": {
// BOUNDBLANK-NEXT:          "start": {
// BOUNDBLANK-NEXT:            "line": 310,
// BOUNDBLANK-NEXT:            "character": 3
// BOUNDBLANK-NEXT:          },
// BOUNDBLANK-NEXT:          "end": {
// BOUNDBLANK-NEXT:            "line": 310,
// BOUNDBLANK-NEXT:            "character": 3
// BOUNDBLANK-NEXT:          }
// BOUNDBLANK-NEXT:        },
// BOUNDBLANK-NEXT:        "text": ""
// BOUNDBLANK-NEXT:      }
// BOUNDBLANK-NEXT:    ]
// BOUNDBLANK-NEXT:  },

///
/// Bound Blank
///
public struct BoundBlank {}
