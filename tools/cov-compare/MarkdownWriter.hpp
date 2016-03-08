//===------- MarkdownWriter.hpp - Tools for analyzing llvm profdata -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef MarkdownWriter_hpp
#define MarkdownWriter_hpp

#include <stdio.h>
#include "ProfdataCompare.hpp"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FormattedStream.h"

namespace covcompare {
  /// A struct that holds a 'column' of information, as represented in a table.
  struct Column {
    /// The alignment of the column,
    /// respected if possible in the output medium.
    typedef enum {
      Left,
      Center,
      Right
    } Alignment;
    
    /// The 'header' at the top of the column in a table.
    std::string header;
    
    /// The individual elements per row of this column.
    std::vector<std::string> elements;
    
    /// The alignment of the values in this column.
    Alignment alignment;
    
    /// A shortcut to add a value to this column.
    void add(std::string val) {
      elements.push_back(val);
    }
    
    Column(std::string header, Alignment alignment = Left,
           std::vector<std::string> elements = {})
    : header(header), elements(elements), alignment(alignment) {}
  };
  
  /// A class that wraps multiple columns and outputs them as a Markdown table.
  class MarkdownWriter {
  public:
    /// The columns this writer will write.
    std::vector<Column> columns;
    
    /// Writes this table to a stream.
    void write(llvm::raw_ostream &os);
    
    MarkdownWriter(std::vector<Column> columns) : columns(columns) {}
  };
}

#endif /* MarkdownWriter_hpp */
