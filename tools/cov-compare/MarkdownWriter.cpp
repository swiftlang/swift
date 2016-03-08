//===------- MarkdownWriter.cpp - Tools for analyzing llvm profdata -------===//
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

#include "MarkdownWriter.hpp"

using namespace llvm;
using namespace std;
namespace covcompare {
  void MarkdownWriter::write(llvm::raw_ostream &os) {
    os << "| ";
    for (auto &column : columns) {
      os << column.header << " | ";
    }
    os << "\n|";
    for (auto &column : columns) {
      auto leftMark = "-";
      auto rightMark = "-";
      switch (column.alignment) {
        case Column::Left:
          leftMark = ":";
          break;
        case Column::Right:
          rightMark = ":";
          break;
        case Column::Center:
          leftMark = ":";
          rightMark = ":";
          break;
      }
      os << leftMark << std::string(column.header.size(), '-') << rightMark
         << "|";
    }
    for (size_t i = 0; i < columns[0].elements.size(); i++) {
      os << "\n| ";
      for (auto &column : columns) {
        os << column.elements[i] << " | ";
      }
    }
  }
}