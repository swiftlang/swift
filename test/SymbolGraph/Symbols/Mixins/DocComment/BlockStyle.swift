// Stop!
// Only add text to the bottom of this file
// or you are going to have a bad time.

// SINGLESAMELINE-LABEL: "precise": "s:10BlockStyle14SingleSameLineV"
// SINGLESAMELINE:       "docComment": {
// SINGLESAMELINE-NEXT:    "lines": [
// SINGLESAMELINE-NEXT:      {
// SINGLESAMELINE-NEXT:        "range": {
// SINGLESAMELINE-NEXT:          "start": {
// SINGLESAMELINE-NEXT:            "line": 23,
// SINGLESAMELINE-NEXT:            "character": 4
// SINGLESAMELINE-NEXT:          },
// SINGLESAMELINE-NEXT:          "end": {
// SINGLESAMELINE-NEXT:            "line": 23,
// SINGLESAMELINE-NEXT:            "character": 21
// SINGLESAMELINE-NEXT:          }
// SINGLESAMELINE-NEXT:        },
// SINGLESAMELINE-NEXT:        "text": "Single Same Line "
// SINGLESAMELINE-NEXT:      }
// SINGLESAMELINE-NEXT:    ]
// SINGLESAMELINE-NEXT:  },

/** Single Same Line */
public struct SingleSameLine {}

// EMPTY-LABEL: "precise": "s:10BlockStyle5EmptyV"
// EMPTY:       "docComment": {
// EMPTY-NEXT:    "lines": []
// EMPTY-NEXT:  },

/***/
public struct Empty {}

// EMPTYWITHNEWLINE-LABEL: "precise": "s:10BlockStyle16EmptyWithNewLineV"
// EMPTHWITHNEWLINE:       "docComment": {
// EMPTHWITHNEWLINE-NEXT:    "lines": [
// EMPTHWITHNEWLINE-NEXT:      {
// EMPTHWITHNEWLINE-NEXT:        "range": {
// EMPTHWITHNEWLINE-NEXT:          "start": {
// EMPTHWITHNEWLINE-NEXT:            "line": 54,
// EMPTHWITHNEWLINE-NEXT:            "character": 1
// EMPTHWITHNEWLINE-NEXT:          },
// EMPTHWITHNEWLINE-NEXT:          "end": {
// EMPTHWITHNEWLINE-NEXT:            "line": 54,
// EMPTHWITHNEWLINE-NEXT:            "character": 1
// EMPTHWITHNEWLINE-NEXT:          }
// EMPTHWITHNEWLINE-NEXT:        },
// EMPTHWITHNEWLINE-NEXT:        "text": ""
// EMPTHWITHNEWLINE-NEXT:      }
// EMPTHWITHNEWLINE-NEXT:    ]
// EMPTHWITHNEWLINE-NEXT:  },

/**
 */
public struct EmptyWithNewLine {}

// SINGLELINE-LABEL: "precise": "s:10BlockStyle10SingleLineV"
// SINGLELINE:       "docComment": {
// SINGLELINE-NEXT:    "lines": [
// SINGLELINE-NEXT:      {
// SINGLELINE-NEXT:        "range": {
// SINGLELINE-NEXT:          "start": {
// SINGLELINE-NEXT:            "line": 90,
// SINGLELINE-NEXT:            "character": 1
// SINGLELINE-NEXT:          },
// SINGLELINE-NEXT:          "end": {
// SINGLELINE-NEXT:            "line": 90,
// SINGLELINE-NEXT:            "character": 13
// SINGLELINE-NEXT:          }
// SINGLELINE-NEXT:        },
// SINGLELINE-NEXT:        "text": "Single line."
// SINGLELINE-NEXT:      },
// SINGLELINE-NEXT:      {
// SINGLELINE-NEXT:        "range": {
// SINGLELINE-NEXT:          "start": {
// SINGLELINE-NEXT:            "line": 91,
// SINGLELINE-NEXT:            "character": 1
// SINGLELINE-NEXT:          },
// SINGLELINE-NEXT:          "end": {
// SINGLELINE-NEXT:            "line": 91,
// SINGLELINE-NEXT:            "character": 1
// SINGLELINE-NEXT:          }
// SINGLELINE-NEXT:        },
// SINGLELINE-NEXT:        "text": ""
// SINGLELINE-NEXT:      }
// SINGLELINE-NEXT:    ]
// SINGLELINE-NEXT:  },

/**
 Single line.
 */
public struct SingleLine {}

// SINGLELINEWITHART-LABEL: "precise": "s:10BlockStyle17SingleLineWithArtV"
// SINGLELINEWITHART:       "docComment": {
// SINGLELINEWITHART-NEXT:    "lines": [
// SINGLELINEWITHART-NEXT:      {
// SINGLELINEWITHART-NEXT:        "range": {
// SINGLELINEWITHART-NEXT:          "start": {
// SINGLELINEWITHART-NEXT:            "line": 127,
// SINGLELINEWITHART-NEXT:            "character": 3
// SINGLELINEWITHART-NEXT:          },
// SINGLELINEWITHART-NEXT:          "end": {
// SINGLELINEWITHART-NEXT:            "line": 127,
// SINGLELINEWITHART-NEXT:            "character": 24
// SINGLELINEWITHART-NEXT:          }
// SINGLELINEWITHART-NEXT:        },
// SINGLELINEWITHART-NEXT:        "text": "Single line with art."
// SINGLELINEWITHART-NEXT:      },
// SINGLELINEWITHART-NEXT:      {
// SINGLELINEWITHART-NEXT:        "range": {
// SINGLELINEWITHART-NEXT:          "start": {
// SINGLELINEWITHART-NEXT:            "line": 128,
// SINGLELINEWITHART-NEXT:            "character": 0
// SINGLELINEWITHART-NEXT:          },
// SINGLELINEWITHART-NEXT:          "end": {
// SINGLELINEWITHART-NEXT:            "line": 128,
// SINGLELINEWITHART-NEXT:            "character": 1
// SINGLELINEWITHART-NEXT:          }
// SINGLELINEWITHART-NEXT:        },
// SINGLELINEWITHART-NEXT:        "text": " "
// SINGLELINEWITHART-NEXT:      }
// SINGLELINEWITHART-NEXT:    ]
// SINGLELINEWITHART-NEXT:  },

/**
 * Single line with art.
 */
public struct SingleLineWithArt {}

// TWOLINES-LABEL: "precise": "s:10BlockStyle8TwoLinesV"
// TWOLINES:       "docComment": {
// TWOLINES-NEXT:    "lines": [
// TWOLINES-NEXT:      {
// TWOLINES-NEXT:        "range": {
// TWOLINES-NEXT:          "start": {
// TWOLINES-NEXT:            "line": 177,
// TWOLINES-NEXT:            "character": 1
// TWOLINES-NEXT:          },
// TWOLINES-NEXT:          "end": {
// TWOLINES-NEXT:            "line": 177,
// TWOLINES-NEXT:            "character": 4
// TWOLINES-NEXT:          }
// TWOLINES-NEXT:        },
// TWOLINES-NEXT:        "text": "Two"
// TWOLINES-NEXT:      },
// TWOLINES-NEXT:      {
// TWOLINES-NEXT:        "range": {
// TWOLINES-NEXT:          "start": {
// TWOLINES-NEXT:            "line": 178,
// TWOLINES-NEXT:            "character": 1
// TWOLINES-NEXT:          },
// TWOLINES-NEXT:          "end": {
// TWOLINES-NEXT:            "line": 178,
// TWOLINES-NEXT:            "character": 7
// TWOLINES-NEXT:          }
// TWOLINES-NEXT:        },
// TWOLINES-NEXT:        "text": "lines."
// TWOLINES-NEXT:      },
// TWOLINES-NEXT:      {
// TWOLINES-NEXT:        "range": {
// TWOLINES-NEXT:          "start": {
// TWOLINES-NEXT:            "line": 179,
// TWOLINES-NEXT:            "character": 1
// TWOLINES-NEXT:          },
// TWOLINES-NEXT:          "end": {
// TWOLINES-NEXT:            "line": 179,
// TWOLINES-NEXT:            "character": 1
// TWOLINES-NEXT:          }
// TWOLINES-NEXT:        },
// TWOLINES-NEXT:        "text": ""
// TWOLINES-NEXT:      }
// TWOLINES-NEXT:    ]
// TWOLINES-NEXT:  },

/**
 Two
 lines.
 */
public struct TwoLines {}

// LARGEINDENT-LABEL: "precise": "s:10BlockStyle11LargeIndentV"
// LARGEINDENT:       "docComment": {
// LARGEINDENT-NEXT:    "lines": [
// LARGEINDENT-NEXT:      {
// LARGEINDENT-NEXT:        "range": {
// LARGEINDENT-NEXT:          "start": {
// LARGEINDENT-NEXT:            "line": 215,
// LARGEINDENT-NEXT:            "character": 5
// LARGEINDENT-NEXT:          },
// LARGEINDENT-NEXT:          "end": {
// LARGEINDENT-NEXT:            "line": 215,
// LARGEINDENT-NEXT:            "character": 17
// LARGEINDENT-NEXT:          }
// LARGEINDENT-NEXT:        },
// LARGEINDENT-NEXT:        "text": "Large Indent"
// LARGEINDENT-NEXT:      },
// LARGEINDENT-NEXT:      {
// LARGEINDENT-NEXT:        "range": {
// LARGEINDENT-NEXT:          "start": {
// LARGEINDENT-NEXT:            "line": 216,
// LARGEINDENT-NEXT:            "character": 1
// LARGEINDENT-NEXT:          },
// LARGEINDENT-NEXT:          "end": {
// LARGEINDENT-NEXT:            "line": 216,
// LARGEINDENT-NEXT:            "character": 1
// LARGEINDENT-NEXT:          }
// LARGEINDENT-NEXT:        },
// LARGEINDENT-NEXT:        "text": ""
// LARGEINDENT-NEXT:      }
// LARGEINDENT-NEXT:    ]
// LARGEINDENT-NEXT:  }

/**
     Large Indent
 */
public struct LargeIndent {}

// TWOLINESBETWEENBLANK-LABEL: "precise": "s:10BlockStyle20TwoLinesBetweenBlankV"
// TWOLINESBETWEENBLANK:       "docComment": {
// TWOLINESBETWEENBLANK-NEXT:    "lines": [
// TWOLINESBETWEENBLANK-NEXT:      {
// TWOLINESBETWEENBLANK-NEXT:        "range": {
// TWOLINESBETWEENBLANK-NEXT:          "start": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 278,
// TWOLINESBETWEENBLANK-NEXT:            "character": 1
// TWOLINESBETWEENBLANK-NEXT:          },
// TWOLINESBETWEENBLANK-NEXT:          "end": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 278,
// TWOLINESBETWEENBLANK-NEXT:            "character": 10
// TWOLINESBETWEENBLANK-NEXT:          }
// TWOLINESBETWEENBLANK-NEXT:        },
// TWOLINESBETWEENBLANK-NEXT:        "text": "Two lines"
// TWOLINESBETWEENBLANK-NEXT:      },
// TWOLINESBETWEENBLANK-NEXT:      {
// TWOLINESBETWEENBLANK-NEXT:        "range": {
// TWOLINESBETWEENBLANK-NEXT:          "start": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 279,
// TWOLINESBETWEENBLANK-NEXT:            "character": 0
// TWOLINESBETWEENBLANK-NEXT:          },
// TWOLINESBETWEENBLANK-NEXT:          "end": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 279,
// TWOLINESBETWEENBLANK-NEXT:            "character": 0
// TWOLINESBETWEENBLANK-NEXT:          }
// TWOLINESBETWEENBLANK-NEXT:        },
// TWOLINESBETWEENBLANK-NEXT:        "text": ""
// TWOLINESBETWEENBLANK-NEXT:      },
// TWOLINESBETWEENBLANK-NEXT:      {
// TWOLINESBETWEENBLANK-NEXT:        "range": {
// TWOLINESBETWEENBLANK-NEXT:          "start": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 280,
// TWOLINESBETWEENBLANK-NEXT:            "character": 1
// TWOLINESBETWEENBLANK-NEXT:          },
// TWOLINESBETWEENBLANK-NEXT:          "end": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 280,
// TWOLINESBETWEENBLANK-NEXT:            "character": 14
// TWOLINESBETWEENBLANK-NEXT:          }
// TWOLINESBETWEENBLANK-NEXT:        },
// TWOLINESBETWEENBLANK-NEXT:        "text": "Between Blank"
// TWOLINESBETWEENBLANK-NEXT:      },
// TWOLINESBETWEENBLANK-NEXT:      {
// TWOLINESBETWEENBLANK-NEXT:        "range": {
// TWOLINESBETWEENBLANK-NEXT:          "start": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 281,
// TWOLINESBETWEENBLANK-NEXT:            "character": 1
// TWOLINESBETWEENBLANK-NEXT:          },
// TWOLINESBETWEENBLANK-NEXT:          "end": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 281,
// TWOLINESBETWEENBLANK-NEXT:            "character": 1
// TWOLINESBETWEENBLANK-NEXT:          }
// TWOLINESBETWEENBLANK-NEXT:        },
// TWOLINESBETWEENBLANK-NEXT:        "text": ""
// TWOLINESBETWEENBLANK-NEXT:      }
// TWOLINESBETWEENBLANK-NEXT:    ]
// TWOLINESBETWEENBLANK-NEXT:  },

/**
 Two lines

 Between Blank
 */
public struct TwoLinesBetweenBlank {}

// LEADINGBLANK-LABEL: "precise": "s:10BlockStyle12LeadingBlankV"
// LEADINGBLANK:       "docComment": {
// LEADINGBLANK-NEXT:    "lines": [
// LEADINGBLANK-NEXT:      {
// LEADINGBLANK-NEXT:        "range": {
// LEADINGBLANK-NEXT:          "start": {
// LEADINGBLANK-NEXT:            "line": 330,
// LEADINGBLANK-NEXT:            "character": 0
// LEADINGBLANK-NEXT:          },
// LEADINGBLANK-NEXT:          "end": {
// LEADINGBLANK-NEXT:            "line": 330,
// LEADINGBLANK-NEXT:            "character": 0
// LEADINGBLANK-NEXT:          }
// LEADINGBLANK-NEXT:        },
// LEADINGBLANK-NEXT:        "text": ""
// LEADINGBLANK-NEXT:      },
// LEADINGBLANK-NEXT:      {
// LEADINGBLANK-NEXT:        "range": {
// LEADINGBLANK-NEXT:          "start": {
// LEADINGBLANK-NEXT:            "line": 331,
// LEADINGBLANK-NEXT:            "character": 1
// LEADINGBLANK-NEXT:          },
// LEADINGBLANK-NEXT:          "end": {
// LEADINGBLANK-NEXT:            "line": 331,
// LEADINGBLANK-NEXT:            "character": 14
// LEADINGBLANK-NEXT:          }
// LEADINGBLANK-NEXT:        },
// LEADINGBLANK-NEXT:        "text": "Leading Blank"
// LEADINGBLANK-NEXT:      },
// LEADINGBLANK-NEXT:      {
// LEADINGBLANK-NEXT:        "range": {
// LEADINGBLANK-NEXT:          "start": {
// LEADINGBLANK-NEXT:            "line": 332,
// LEADINGBLANK-NEXT:            "character": 1
// LEADINGBLANK-NEXT:          },
// LEADINGBLANK-NEXT:          "end": {
// LEADINGBLANK-NEXT:            "line": 332,
// LEADINGBLANK-NEXT:            "character": 1
// LEADINGBLANK-NEXT:          }
// LEADINGBLANK-NEXT:        },
// LEADINGBLANK-NEXT:        "text": ""
// LEADINGBLANK-NEXT:      }
// LEADINGBLANK-NEXT:    ]
// LEADINGBLANK-NEXT:  },

/**

 Leading Blank
 */
public struct LeadingBlank {}

// TRAILINGBLANK-LABEL: "precise": "s:10BlockStyle13TrailingBlankV"
// TRAILINGBLANK:       "docComment": {
// TRAILINGBLANK-NEXT:    "lines": [
// TRAILINGBLANK-NEXT:      {
// TRAILINGBLANK-NEXT:        "range": {
// TRAILINGBLANK-NEXT:          "start": {
// TRAILINGBLANK-NEXT:            "line": 381,
// TRAILINGBLANK-NEXT:            "character": 1
// TRAILINGBLANK-NEXT:          },
// TRAILINGBLANK-NEXT:          "end": {
// TRAILINGBLANK-NEXT:            "line": 381,
// TRAILINGBLANK-NEXT:            "character": 15
// TRAILINGBLANK-NEXT:          }
// TRAILINGBLANK-NEXT:        },
// TRAILINGBLANK-NEXT:        "text": "Trailing Blank"
// TRAILINGBLANK-NEXT:      },
// TRAILINGBLANK-NEXT:      {
// TRAILINGBLANK-NEXT:        "range": {
// TRAILINGBLANK-NEXT:          "start": {
// TRAILINGBLANK-NEXT:            "line": 382,
// TRAILINGBLANK-NEXT:            "character": 0
// TRAILINGBLANK-NEXT:          },
// TRAILINGBLANK-NEXT:          "end": {
// TRAILINGBLANK-NEXT:            "line": 382,
// TRAILINGBLANK-NEXT:            "character": 0
// TRAILINGBLANK-NEXT:          }
// TRAILINGBLANK-NEXT:        },
// TRAILINGBLANK-NEXT:        "text": ""
// TRAILINGBLANK-NEXT:      },
// TRAILINGBLANK-NEXT:      {
// TRAILINGBLANK-NEXT:        "range": {
// TRAILINGBLANK-NEXT:          "start": {
// TRAILINGBLANK-NEXT:            "line": 383,
// TRAILINGBLANK-NEXT:            "character": 1
// TRAILINGBLANK-NEXT:          },
// TRAILINGBLANK-NEXT:          "end": {
// TRAILINGBLANK-NEXT:            "line": 383,
// TRAILINGBLANK-NEXT:            "character": 1
// TRAILINGBLANK-NEXT:          }
// TRAILINGBLANK-NEXT:        },
// TRAILINGBLANK-NEXT:        "text": ""
// TRAILINGBLANK-NEXT:      }
// TRAILINGBLANK-NEXT:    ]
// TRAILINGBLANK-NEXT:  },

/**
 Trailing Blank

 */
public struct TrailingBlank {}

// BOUNDBLANK-LABEL: "precise": "s:10BlockStyle10BoundBlankV"
// BOUNDBLANK:       "docComment": {
// BOUNDBLANK-NEXT:    "lines": [
// BOUNDBLANK-NEXT:      {
// BOUNDBLANK-NEXT:        "range": {
// BOUNDBLANK-NEXT:          "start": {
// BOUNDBLANK-NEXT:            "line": 445,
// BOUNDBLANK-NEXT:            "character": 0
// BOUNDBLANK-NEXT:          },
// BOUNDBLANK-NEXT:          "end": {
// BOUNDBLANK-NEXT:            "line": 445,
// BOUNDBLANK-NEXT:            "character": 0
// BOUNDBLANK-NEXT:          }
// BOUNDBLANK-NEXT:        },
// BOUNDBLANK-NEXT:        "text": ""
// BOUNDBLANK-NEXT:      },
// BOUNDBLANK-NEXT:      {
// BOUNDBLANK-NEXT:        "range": {
// BOUNDBLANK-NEXT:          "start": {
// BOUNDBLANK-NEXT:            "line": 446,
// BOUNDBLANK-NEXT:            "character": 1
// BOUNDBLANK-NEXT:          },
// BOUNDBLANK-NEXT:          "end": {
// BOUNDBLANK-NEXT:            "line": 446,
// BOUNDBLANK-NEXT:            "character": 12
// BOUNDBLANK-NEXT:          }
// BOUNDBLANK-NEXT:        },
// BOUNDBLANK-NEXT:        "text": "Bound Blank"
// BOUNDBLANK-NEXT:      },
// BOUNDBLANK-NEXT:      {
// BOUNDBLANK-NEXT:        "range": {
// BOUNDBLANK-NEXT:          "start": {
// BOUNDBLANK-NEXT:            "line": 447,
// BOUNDBLANK-NEXT:            "character": 0
// BOUNDBLANK-NEXT:          },
// BOUNDBLANK-NEXT:          "end": {
// BOUNDBLANK-NEXT:            "line": 447,
// BOUNDBLANK-NEXT:            "character": 0
// BOUNDBLANK-NEXT:          }
// BOUNDBLANK-NEXT:        },
// BOUNDBLANK-NEXT:        "text": ""
// BOUNDBLANK-NEXT:      },
// BOUNDBLANK-NEXT:      {
// BOUNDBLANK-NEXT:        "range": {
// BOUNDBLANK-NEXT:          "start": {
// BOUNDBLANK-NEXT:            "line": 448,
// BOUNDBLANK-NEXT:            "character": 1
// BOUNDBLANK-NEXT:          },
// BOUNDBLANK-NEXT:          "end": {
// BOUNDBLANK-NEXT:            "line": 448,
// BOUNDBLANK-NEXT:            "character": 1
// BOUNDBLANK-NEXT:          }
// BOUNDBLANK-NEXT:        },
// BOUNDBLANK-NEXT:        "text": ""
// BOUNDBLANK-NEXT:      }
// BOUNDBLANK-NEXT:    ]
// BOUNDBLANK-NEXT:  },

/**

 Bound Blank

 */
public struct BoundBlank {}

// ALLINDENTED-LABEL: "precise": "s:10BlockStyle11AllIndentedV"
// ALLINDENTED:       "docComment": {
// ALLINDENTED-NEXT:    "lines": [
// ALLINDENTED-NEXT:      {
// ALLINDENTED-NEXT:        "range": {
// ALLINDENTED-NEXT:          "start": {
// ALLINDENTED-NEXT:            "line": 484,
// ALLINDENTED-NEXT:            "character": 5
// ALLINDENTED-NEXT:          },
// ALLINDENTED-NEXT:          "end": {
// ALLINDENTED-NEXT:            "line": 484,
// ALLINDENTED-NEXT:            "character": 18
// ALLINDENTED-NEXT:          }
// ALLINDENTED-NEXT:        },
// ALLINDENTED-NEXT:        "text": "All indented."
// ALLINDENTED-NEXT:      },
// ALLINDENTED-NEXT:      {
// ALLINDENTED-NEXT:        "range": {
// ALLINDENTED-NEXT:          "start": {
// ALLINDENTED-NEXT:            "line": 485,
// ALLINDENTED-NEXT:            "character": 5
// ALLINDENTED-NEXT:          },
// ALLINDENTED-NEXT:          "end": {
// ALLINDENTED-NEXT:            "line": 485,
// ALLINDENTED-NEXT:            "character": 5
// ALLINDENTED-NEXT:          }
// ALLINDENTED-NEXT:        },
// ALLINDENTED-NEXT:        "text": ""
// ALLINDENTED-NEXT:      }
// ALLINDENTED-NEXT:    ]
// ALLINDENTED-NEXT:  },

    /**
     All indented.
     */
     public struct AllIndented {}

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name BlockStyle -emit-module-path %t/BlockStyle.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name BlockStyle -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/BlockStyle.symbols.json --check-prefix=SINGLESAMELINE
// RUN: %FileCheck %s --input-file %t/BlockStyle.symbols.json --check-prefix=EMPTY
// RUN: %FileCheck %s --input-file %t/BlockStyle.symbols.json --check-prefix=EMPTYWITHNEWLINE
// RUN: %FileCheck %s --input-file %t/BlockStyle.symbols.json --check-prefix=SINGLELINE
// RUN: %FileCheck %s --input-file %t/BlockStyle.symbols.json --check-prefix=SINGLELINEWITHART
// RUN: %FileCheck %s --input-file %t/BlockStyle.symbols.json --check-prefix=LARGEINDENT
// RUN: %FileCheck %s --input-file %t/BlockStyle.symbols.json --check-prefix=TWOLINESBETWEENBLANK
// RUN: %FileCheck %s --input-file %t/BlockStyle.symbols.json --check-prefix=LEADINGBLANK
// RUN: %FileCheck %s --input-file %t/BlockStyle.symbols.json --check-prefix=TRAILINGBLANK
