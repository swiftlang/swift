// Stop!
// Only add text to the bottom of this file
// or you are going to have a bad time.

/** Single Same Line */
public struct SingleSameLine {}

/***/
public struct Empty {}

/**
 */
public struct EmptyWithNewLine {}

/**
 Single line.
 */
public struct SingleLine {}

/**
 * Single line with art.
 */
public struct SingleLineWithArt {}

/**
 Two
 lines.
 */
public struct TwoLines {}

/**
     Large Indent
 */
public struct LargeIndent {}

/**
 Two lines

 Between Blank
 */
public struct TwoLinesBetweenBlank {}

/**

 Leading Blank
 */
public struct LeadingBlank {}

/**
 Trailing Blank

 */
public struct TrailingBlank {}

/**

 Bound Blank

 */
public struct BoundBlank {}

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

// SINGLESAMELINE-LABEL: "precise": "s:10BlockStyle14SingleSameLineV"
// SINGLESAMELINE:       "docComment": {
// SINGLESAMELINE-NEXT:    "uri": "file://{{.*}}BlockStyle.swift",
// SINGLESAMELINE-NEXT:    "module": "BlockStyle",
// SINGLESAMELINE-NEXT:    "lines": [
// SINGLESAMELINE-NEXT:      {
// SINGLESAMELINE-NEXT:        "range": {
// SINGLESAMELINE-NEXT:          "start": {
// SINGLESAMELINE-NEXT:            "line": 4,
// SINGLESAMELINE-NEXT:            "character": 4
// SINGLESAMELINE-NEXT:          },
// SINGLESAMELINE-NEXT:          "end": {
// SINGLESAMELINE-NEXT:            "line": 4,
// SINGLESAMELINE-NEXT:            "character": 21
// SINGLESAMELINE-NEXT:          }
// SINGLESAMELINE-NEXT:        },
// SINGLESAMELINE-NEXT:        "text": "Single Same Line "
// SINGLESAMELINE-NEXT:      }
// SINGLESAMELINE-NEXT:    ]
// SINGLESAMELINE-NEXT:  },

// EMPTY-LABEL: "precise": "s:10BlockStyle5EmptyV"
// EMPTY:       "docComment": {
// EMPTY-NEXT:    "uri": "file://{{.*}}BlockStyle.swift",
// EMPTY-NEXT:    "module": "BlockStyle",
// EMPTY-NEXT:    "lines": []
// EMPTY-NEXT:  },

// EMPTYWITHNEWLINE-LABEL: "precise": "s:10BlockStyle16EmptyWithNewLineV"
// EMPTYWITHNEWLINE:       "docComment": {
// EMPTYWITHNEWLINE-NEXT:    "uri": "file://{{.*}}BlockStyle.swift",
// EMPTYWITHNEWLINE-NEXT:    "module": "BlockStyle",
// EMPTYWITHNEWLINE-NEXT:    "lines": [
// EMPTYWITHNEWLINE-NEXT:      {
// EMPTYWITHNEWLINE-NEXT:        "range": {
// EMPTYWITHNEWLINE-NEXT:          "start": {
// EMPTYWITHNEWLINE-NEXT:            "line": 11,
// EMPTYWITHNEWLINE-NEXT:            "character": 1
// EMPTYWITHNEWLINE-NEXT:          },
// EMPTYWITHNEWLINE-NEXT:          "end": {
// EMPTYWITHNEWLINE-NEXT:            "line": 11,
// EMPTYWITHNEWLINE-NEXT:            "character": 1
// EMPTYWITHNEWLINE-NEXT:          }
// EMPTYWITHNEWLINE-NEXT:        },
// EMPTYWITHNEWLINE-NEXT:        "text": ""
// EMPTYWITHNEWLINE-NEXT:      }
// EMPTYWITHNEWLINE-NEXT:    ]
// EMPTYWITHNEWLINE-NEXT:  },

// SINGLELINE-LABEL: "precise": "s:10BlockStyle10SingleLineV"
// SINGLELINE:       "docComment": {
// SINGLELINE-NEXT:    "uri": "file://{{.*}}BlockStyle.swift",
// SINGLELINE-NEXT:    "module": "BlockStyle",
// SINGLELINE-NEXT:    "lines": [
// SINGLELINE-NEXT:      {
// SINGLELINE-NEXT:        "range": {
// SINGLELINE-NEXT:          "start": {
// SINGLELINE-NEXT:            "line": 15,
// SINGLELINE-NEXT:            "character": 1
// SINGLELINE-NEXT:          },
// SINGLELINE-NEXT:          "end": {
// SINGLELINE-NEXT:            "line": 15,
// SINGLELINE-NEXT:            "character": 13
// SINGLELINE-NEXT:          }
// SINGLELINE-NEXT:        },
// SINGLELINE-NEXT:        "text": "Single line."
// SINGLELINE-NEXT:      },
// SINGLELINE-NEXT:      {
// SINGLELINE-NEXT:        "range": {
// SINGLELINE-NEXT:          "start": {
// SINGLELINE-NEXT:            "line": 16,
// SINGLELINE-NEXT:            "character": 1
// SINGLELINE-NEXT:          },
// SINGLELINE-NEXT:          "end": {
// SINGLELINE-NEXT:            "line": 16,
// SINGLELINE-NEXT:            "character": 1
// SINGLELINE-NEXT:          }
// SINGLELINE-NEXT:        },
// SINGLELINE-NEXT:        "text": ""
// SINGLELINE-NEXT:      }
// SINGLELINE-NEXT:    ]
// SINGLELINE-NEXT:  },

// SINGLELINEWITHART-LABEL: "precise": "s:10BlockStyle17SingleLineWithArtV"
// SINGLELINEWITHART:       "docComment": {
// SINGLELINEWITHART-NEXT:    "uri": "file://{{.*}}BlockStyle.swift",
// SINGLELINEWITHART-NEXT:    "module": "BlockStyle",
// SINGLELINEWITHART-NEXT:    "lines": [
// SINGLELINEWITHART-NEXT:      {
// SINGLELINEWITHART-NEXT:        "range": {
// SINGLELINEWITHART-NEXT:          "start": {
// SINGLELINEWITHART-NEXT:            "line": 20,
// SINGLELINEWITHART-NEXT:            "character": 3
// SINGLELINEWITHART-NEXT:          },
// SINGLELINEWITHART-NEXT:          "end": {
// SINGLELINEWITHART-NEXT:            "line": 20,
// SINGLELINEWITHART-NEXT:            "character": 24
// SINGLELINEWITHART-NEXT:          }
// SINGLELINEWITHART-NEXT:        },
// SINGLELINEWITHART-NEXT:        "text": "Single line with art."
// SINGLELINEWITHART-NEXT:      },
// SINGLELINEWITHART-NEXT:      {
// SINGLELINEWITHART-NEXT:        "range": {
// SINGLELINEWITHART-NEXT:          "start": {
// SINGLELINEWITHART-NEXT:            "line": 21,
// SINGLELINEWITHART-NEXT:            "character": 0
// SINGLELINEWITHART-NEXT:          },
// SINGLELINEWITHART-NEXT:          "end": {
// SINGLELINEWITHART-NEXT:            "line": 21,
// SINGLELINEWITHART-NEXT:            "character": 1
// SINGLELINEWITHART-NEXT:          }
// SINGLELINEWITHART-NEXT:        },
// SINGLELINEWITHART-NEXT:        "text": " "
// SINGLELINEWITHART-NEXT:      }
// SINGLELINEWITHART-NEXT:    ]
// SINGLELINEWITHART-NEXT:  },

// TWOLINES-LABEL: "precise": "s:10BlockStyle8TwoLinesV"
// TWOLINES:       "docComment": {
// TWOLINES-NEXT:    "uri": "file://{{.*}}BlockStyle.swift",
// TWOLINES-NEXT:    "module": "BlockStyle",
// TWOLINES-NEXT:    "lines": [
// TWOLINES-NEXT:      {
// TWOLINES-NEXT:        "range": {
// TWOLINES-NEXT:          "start": {
// TWOLINES-NEXT:            "line": 25,
// TWOLINES-NEXT:            "character": 1
// TWOLINES-NEXT:          },
// TWOLINES-NEXT:          "end": {
// TWOLINES-NEXT:            "line": 25,
// TWOLINES-NEXT:            "character": 4
// TWOLINES-NEXT:          }
// TWOLINES-NEXT:        },
// TWOLINES-NEXT:        "text": "Two"
// TWOLINES-NEXT:      },
// TWOLINES-NEXT:      {
// TWOLINES-NEXT:        "range": {
// TWOLINES-NEXT:          "start": {
// TWOLINES-NEXT:            "line": 26,
// TWOLINES-NEXT:            "character": 1
// TWOLINES-NEXT:          },
// TWOLINES-NEXT:          "end": {
// TWOLINES-NEXT:            "line": 26,
// TWOLINES-NEXT:            "character": 7
// TWOLINES-NEXT:          }
// TWOLINES-NEXT:        },
// TWOLINES-NEXT:        "text": "lines."
// TWOLINES-NEXT:      },
// TWOLINES-NEXT:      {
// TWOLINES-NEXT:        "range": {
// TWOLINES-NEXT:          "start": {
// TWOLINES-NEXT:            "line": 27,
// TWOLINES-NEXT:            "character": 1
// TWOLINES-NEXT:          },
// TWOLINES-NEXT:          "end": {
// TWOLINES-NEXT:            "line": 27,
// TWOLINES-NEXT:            "character": 1
// TWOLINES-NEXT:          }
// TWOLINES-NEXT:        },
// TWOLINES-NEXT:        "text": ""
// TWOLINES-NEXT:      }
// TWOLINES-NEXT:    ]
// TWOLINES-NEXT:  },

// LARGEINDENT-LABEL: "precise": "s:10BlockStyle11LargeIndentV"
// LARGEINDENT:       "docComment": {
// LARGEINDENT-NEXT:    "uri": "file://{{.*}}BlockStyle.swift",
// LARGEINDENT-NEXT:    "module": "BlockStyle",
// LARGEINDENT-NEXT:    "lines": [
// LARGEINDENT-NEXT:      {
// LARGEINDENT-NEXT:        "range": {
// LARGEINDENT-NEXT:          "start": {
// LARGEINDENT-NEXT:            "line": 31,
// LARGEINDENT-NEXT:            "character": 5
// LARGEINDENT-NEXT:          },
// LARGEINDENT-NEXT:          "end": {
// LARGEINDENT-NEXT:            "line": 31,
// LARGEINDENT-NEXT:            "character": 17
// LARGEINDENT-NEXT:          }
// LARGEINDENT-NEXT:        },
// LARGEINDENT-NEXT:        "text": "Large Indent"
// LARGEINDENT-NEXT:      },
// LARGEINDENT-NEXT:      {
// LARGEINDENT-NEXT:        "range": {
// LARGEINDENT-NEXT:          "start": {
// LARGEINDENT-NEXT:            "line": 32,
// LARGEINDENT-NEXT:            "character": 1
// LARGEINDENT-NEXT:          },
// LARGEINDENT-NEXT:          "end": {
// LARGEINDENT-NEXT:            "line": 32,
// LARGEINDENT-NEXT:            "character": 1
// LARGEINDENT-NEXT:          }
// LARGEINDENT-NEXT:        },
// LARGEINDENT-NEXT:        "text": ""
// LARGEINDENT-NEXT:      }
// LARGEINDENT-NEXT:    ]
// LARGEINDENT-NEXT:  }

// TWOLINESBETWEENBLANK-LABEL: "precise": "s:10BlockStyle20TwoLinesBetweenBlankV"
// TWOLINESBETWEENBLANK:       "docComment": {
// TWOLINESBETWEENBLANK-NEXT:    "uri": "file://{{.*}}BlockStyle.swift",
// TWOLINESBETWEENBLANK-NEXT:    "module": "BlockStyle",
// TWOLINESBETWEENBLANK-NEXT:    "lines": [
// TWOLINESBETWEENBLANK-NEXT:      {
// TWOLINESBETWEENBLANK-NEXT:        "range": {
// TWOLINESBETWEENBLANK-NEXT:          "start": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 36,
// TWOLINESBETWEENBLANK-NEXT:            "character": 1
// TWOLINESBETWEENBLANK-NEXT:          },
// TWOLINESBETWEENBLANK-NEXT:          "end": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 36,
// TWOLINESBETWEENBLANK-NEXT:            "character": 10
// TWOLINESBETWEENBLANK-NEXT:          }
// TWOLINESBETWEENBLANK-NEXT:        },
// TWOLINESBETWEENBLANK-NEXT:        "text": "Two lines"
// TWOLINESBETWEENBLANK-NEXT:      },
// TWOLINESBETWEENBLANK-NEXT:      {
// TWOLINESBETWEENBLANK-NEXT:        "range": {
// TWOLINESBETWEENBLANK-NEXT:          "start": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 37,
// TWOLINESBETWEENBLANK-NEXT:            "character": 0
// TWOLINESBETWEENBLANK-NEXT:          },
// TWOLINESBETWEENBLANK-NEXT:          "end": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 37,
// TWOLINESBETWEENBLANK-NEXT:            "character": 0
// TWOLINESBETWEENBLANK-NEXT:          }
// TWOLINESBETWEENBLANK-NEXT:        },
// TWOLINESBETWEENBLANK-NEXT:        "text": ""
// TWOLINESBETWEENBLANK-NEXT:      },
// TWOLINESBETWEENBLANK-NEXT:      {
// TWOLINESBETWEENBLANK-NEXT:        "range": {
// TWOLINESBETWEENBLANK-NEXT:          "start": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 38,
// TWOLINESBETWEENBLANK-NEXT:            "character": 1
// TWOLINESBETWEENBLANK-NEXT:          },
// TWOLINESBETWEENBLANK-NEXT:          "end": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 38,
// TWOLINESBETWEENBLANK-NEXT:            "character": 14
// TWOLINESBETWEENBLANK-NEXT:          }
// TWOLINESBETWEENBLANK-NEXT:        },
// TWOLINESBETWEENBLANK-NEXT:        "text": "Between Blank"
// TWOLINESBETWEENBLANK-NEXT:      },
// TWOLINESBETWEENBLANK-NEXT:      {
// TWOLINESBETWEENBLANK-NEXT:        "range": {
// TWOLINESBETWEENBLANK-NEXT:          "start": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 39,
// TWOLINESBETWEENBLANK-NEXT:            "character": 1
// TWOLINESBETWEENBLANK-NEXT:          },
// TWOLINESBETWEENBLANK-NEXT:          "end": {
// TWOLINESBETWEENBLANK-NEXT:            "line": 39,
// TWOLINESBETWEENBLANK-NEXT:            "character": 1
// TWOLINESBETWEENBLANK-NEXT:          }
// TWOLINESBETWEENBLANK-NEXT:        },
// TWOLINESBETWEENBLANK-NEXT:        "text": ""
// TWOLINESBETWEENBLANK-NEXT:      }
// TWOLINESBETWEENBLANK-NEXT:    ]
// TWOLINESBETWEENBLANK-NEXT:  },

// LEADINGBLANK-LABEL: "precise": "s:10BlockStyle12LeadingBlankV"
// LEADINGBLANK:       "docComment": {
// LEADINGBLANK-NEXT:    "uri": "file://{{.*}}BlockStyle.swift",
// LEADINGBLANK-NEXT:    "module": "BlockStyle",
// LEADINGBLANK-NEXT:    "lines": [
// LEADINGBLANK-NEXT:      {
// LEADINGBLANK-NEXT:        "range": {
// LEADINGBLANK-NEXT:          "start": {
// LEADINGBLANK-NEXT:            "line": 43,
// LEADINGBLANK-NEXT:            "character": 0
// LEADINGBLANK-NEXT:          },
// LEADINGBLANK-NEXT:          "end": {
// LEADINGBLANK-NEXT:            "line": 43,
// LEADINGBLANK-NEXT:            "character": 0
// LEADINGBLANK-NEXT:          }
// LEADINGBLANK-NEXT:        },
// LEADINGBLANK-NEXT:        "text": ""
// LEADINGBLANK-NEXT:      },
// LEADINGBLANK-NEXT:      {
// LEADINGBLANK-NEXT:        "range": {
// LEADINGBLANK-NEXT:          "start": {
// LEADINGBLANK-NEXT:            "line": 44,
// LEADINGBLANK-NEXT:            "character": 1
// LEADINGBLANK-NEXT:          },
// LEADINGBLANK-NEXT:          "end": {
// LEADINGBLANK-NEXT:            "line": 44,
// LEADINGBLANK-NEXT:            "character": 14
// LEADINGBLANK-NEXT:          }
// LEADINGBLANK-NEXT:        },
// LEADINGBLANK-NEXT:        "text": "Leading Blank"
// LEADINGBLANK-NEXT:      },
// LEADINGBLANK-NEXT:      {
// LEADINGBLANK-NEXT:        "range": {
// LEADINGBLANK-NEXT:          "start": {
// LEADINGBLANK-NEXT:            "line": 45,
// LEADINGBLANK-NEXT:            "character": 1
// LEADINGBLANK-NEXT:          },
// LEADINGBLANK-NEXT:          "end": {
// LEADINGBLANK-NEXT:            "line": 45,
// LEADINGBLANK-NEXT:            "character": 1
// LEADINGBLANK-NEXT:          }
// LEADINGBLANK-NEXT:        },
// LEADINGBLANK-NEXT:        "text": ""
// LEADINGBLANK-NEXT:      }
// LEADINGBLANK-NEXT:    ]
// LEADINGBLANK-NEXT:  },

// TRAILINGBLANK-LABEL: "precise": "s:10BlockStyle13TrailingBlankV"
// TRAILINGBLANK:       "docComment": {
// TRAILINGBLANK-NEXT:    "uri": "file://{{.*}}BlockStyle.swift",
// TRAILINGBLANK-NEXT:    "module": "BlockStyle",
// TRAILINGBLANK-NEXT:    "lines": [
// TRAILINGBLANK-NEXT:      {
// TRAILINGBLANK-NEXT:        "range": {
// TRAILINGBLANK-NEXT:          "start": {
// TRAILINGBLANK-NEXT:            "line": 49,
// TRAILINGBLANK-NEXT:            "character": 1
// TRAILINGBLANK-NEXT:          },
// TRAILINGBLANK-NEXT:          "end": {
// TRAILINGBLANK-NEXT:            "line": 49,
// TRAILINGBLANK-NEXT:            "character": 15
// TRAILINGBLANK-NEXT:          }
// TRAILINGBLANK-NEXT:        },
// TRAILINGBLANK-NEXT:        "text": "Trailing Blank"
// TRAILINGBLANK-NEXT:      },
// TRAILINGBLANK-NEXT:      {
// TRAILINGBLANK-NEXT:        "range": {
// TRAILINGBLANK-NEXT:          "start": {
// TRAILINGBLANK-NEXT:            "line": 50,
// TRAILINGBLANK-NEXT:            "character": 0
// TRAILINGBLANK-NEXT:          },
// TRAILINGBLANK-NEXT:          "end": {
// TRAILINGBLANK-NEXT:            "line": 50,
// TRAILINGBLANK-NEXT:            "character": 0
// TRAILINGBLANK-NEXT:          }
// TRAILINGBLANK-NEXT:        },
// TRAILINGBLANK-NEXT:        "text": ""
// TRAILINGBLANK-NEXT:      },
// TRAILINGBLANK-NEXT:      {
// TRAILINGBLANK-NEXT:        "range": {
// TRAILINGBLANK-NEXT:          "start": {
// TRAILINGBLANK-NEXT:            "line": 51,
// TRAILINGBLANK-NEXT:            "character": 1
// TRAILINGBLANK-NEXT:          },
// TRAILINGBLANK-NEXT:          "end": {
// TRAILINGBLANK-NEXT:            "line": 51,
// TRAILINGBLANK-NEXT:            "character": 1
// TRAILINGBLANK-NEXT:          }
// TRAILINGBLANK-NEXT:        },
// TRAILINGBLANK-NEXT:        "text": ""
// TRAILINGBLANK-NEXT:      }
// TRAILINGBLANK-NEXT:    ]
// TRAILINGBLANK-NEXT:  },

// BOUNDBLANK-LABEL: "precise": "s:10BlockStyle10BoundBlankV"
// BOUNDBLANK:       "docComment": {
// BOUNDBLANK-NEXT:    "uri": "file://{{.*}}BlockStyle.swift",
// BOUNDBLANK-NEXT:    "module": "BlockStyle",
// BOUNDBLANK-NEXT:    "lines": [
// BOUNDBLANK-NEXT:      {
// BOUNDBLANK-NEXT:        "range": {
// BOUNDBLANK-NEXT:          "start": {
// BOUNDBLANK-NEXT:            "line": 55,
// BOUNDBLANK-NEXT:            "character": 0
// BOUNDBLANK-NEXT:          },
// BOUNDBLANK-NEXT:          "end": {
// BOUNDBLANK-NEXT:            "line": 55,
// BOUNDBLANK-NEXT:            "character": 0
// BOUNDBLANK-NEXT:          }
// BOUNDBLANK-NEXT:        },
// BOUNDBLANK-NEXT:        "text": ""
// BOUNDBLANK-NEXT:      },
// BOUNDBLANK-NEXT:      {
// BOUNDBLANK-NEXT:        "range": {
// BOUNDBLANK-NEXT:          "start": {
// BOUNDBLANK-NEXT:            "line": 56,
// BOUNDBLANK-NEXT:            "character": 1
// BOUNDBLANK-NEXT:          },
// BOUNDBLANK-NEXT:          "end": {
// BOUNDBLANK-NEXT:            "line": 56,
// BOUNDBLANK-NEXT:            "character": 12
// BOUNDBLANK-NEXT:          }
// BOUNDBLANK-NEXT:        },
// BOUNDBLANK-NEXT:        "text": "Bound Blank"
// BOUNDBLANK-NEXT:      },
// BOUNDBLANK-NEXT:      {
// BOUNDBLANK-NEXT:        "range": {
// BOUNDBLANK-NEXT:          "start": {
// BOUNDBLANK-NEXT:            "line": 57,
// BOUNDBLANK-NEXT:            "character": 0
// BOUNDBLANK-NEXT:          },
// BOUNDBLANK-NEXT:          "end": {
// BOUNDBLANK-NEXT:            "line": 57,
// BOUNDBLANK-NEXT:            "character": 0
// BOUNDBLANK-NEXT:          }
// BOUNDBLANK-NEXT:        },
// BOUNDBLANK-NEXT:        "text": ""
// BOUNDBLANK-NEXT:      },
// BOUNDBLANK-NEXT:      {
// BOUNDBLANK-NEXT:        "range": {
// BOUNDBLANK-NEXT:          "start": {
// BOUNDBLANK-NEXT:            "line": 58,
// BOUNDBLANK-NEXT:            "character": 1
// BOUNDBLANK-NEXT:          },
// BOUNDBLANK-NEXT:          "end": {
// BOUNDBLANK-NEXT:            "line": 58,
// BOUNDBLANK-NEXT:            "character": 1
// BOUNDBLANK-NEXT:          }
// BOUNDBLANK-NEXT:        },
// BOUNDBLANK-NEXT:        "text": ""
// BOUNDBLANK-NEXT:      }
// BOUNDBLANK-NEXT:    ]
// BOUNDBLANK-NEXT:  },

// ALLINDENTED-LABEL: "precise": "s:10BlockStyle11AllIndentedV"
// ALLINDENTED:       "docComment": {
// ALLINDENTED-NEXT:    "uri": "file://{{.*}}BlockStyle.swift",
// ALLINDENTED-NEXT:    "module": "BlockStyle",
// ALLINDENTED-NEXT:    "lines": [
// ALLINDENTED-NEXT:      {
// ALLINDENTED-NEXT:        "range": {
// ALLINDENTED-NEXT:          "start": {
// ALLINDENTED-NEXT:            "line": 62,
// ALLINDENTED-NEXT:            "character": 5
// ALLINDENTED-NEXT:          },
// ALLINDENTED-NEXT:          "end": {
// ALLINDENTED-NEXT:            "line": 62,
// ALLINDENTED-NEXT:            "character": 18
// ALLINDENTED-NEXT:          }
// ALLINDENTED-NEXT:        },
// ALLINDENTED-NEXT:        "text": "All indented."
// ALLINDENTED-NEXT:      },
// ALLINDENTED-NEXT:      {
// ALLINDENTED-NEXT:        "range": {
// ALLINDENTED-NEXT:          "start": {
// ALLINDENTED-NEXT:            "line": 63,
// ALLINDENTED-NEXT:            "character": 5
// ALLINDENTED-NEXT:          },
// ALLINDENTED-NEXT:          "end": {
// ALLINDENTED-NEXT:            "line": 63,
// ALLINDENTED-NEXT:            "character": 5
// ALLINDENTED-NEXT:          }
// ALLINDENTED-NEXT:        },
// ALLINDENTED-NEXT:        "text": ""
// ALLINDENTED-NEXT:      }
// ALLINDENTED-NEXT:    ]
// ALLINDENTED-NEXT:  },
