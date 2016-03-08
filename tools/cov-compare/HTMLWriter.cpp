//===--------- HTMLWriter.cpp - Tools for comparing llvm profdata ---------===//
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

#include "HTMLWriter.hpp"
#include "MarkdownWriter.hpp"
#include "ProfdataCompare.hpp"
#include <libgen.h>
#include <sys/stat.h>
#include <chrono>

using namespace std;
using namespace llvm;
namespace html {
  /// \returns An HTML-escaped string.
  string escape(string s) {
    string result;
    for (size_t i = 0; i < s.size(); ++i) {
      string token = s.substr(i,1);
      if (token == "&")
        token = "&amp;";
      else if (token == "<")
        token = "&lt;";
      else if (token == "\"")
        token = "&quot;";
      else if (token == ">")
        token = "&gt;";
      result += token;
    }
    return result;
  }
  
  /// \returns A tag around the provided text
  /// (expects the value to be properly escaped).
  string tag(string name, string text) {
    return "<" + name + ">" + text + "</" + name + ">";
  }
  
  /// \returns An anchor tag with the provided destination and text.
  string a(string dest, string text) {
    return "<a href='" + dest + "'>" + text + "</a>";
  }
  
  /// \returns Headers of an HTML table, corresponding to the passed-in strings.
  string headerRow(vector<string> headers) {
    string final;
    for (auto &val : headers) {
      final += tag("th", val);
    }
    return tag("tr", final);
  }
  
  /// \returns An HTML table row with the provided strings as td rows inside.
  string tr(vector<string> data) {
    string final;
    for (auto &val : data) {
      final += tag("td", val);
    }
    return tag("tr", final);
  }
  
  /// \returns A span with the provided class, with the provided text inside.
  string span(string _class, string text) {
    return "<span class='" + _class + "'>" + text + "</span>";
  }
}

namespace covcompare {
  typedef enum {
    Bad,
    Warning,
    Good
  } CoverageStatus;
  
  /// \returns The class name corresponding to a CoverageStatus.
  string coverageStatusString(CoverageStatus status) {
    switch (status) {
      case Bad:
        return "bad";
      case Warning:
        return "warning";
      case Good:
        return "good";
    }
  }
  
  CoverageStatus statusForDiff(double diff) {
    bool isSmallDiff = fabs(diff) < 5.0;
    bool isNegative = diff < 0;
    if (isNegative) {
      return isSmallDiff ? Warning : Bad;
    } else {
      return Good;
    }
  }
  
  void HTMLWriter::writeTable(vector<Column> columns, llvm::raw_ostream &os) {
    os << "<table>";
    vector<string> headers;
    for (auto &column : columns) {
      headers.push_back(column.header);
    }
    os << html::headerRow(headers);
    for (size_t i = 0; i < columns[0].elements.size(); i++) {
      vector<string> elements;
      for (auto &column : columns) {
        elements.push_back(column.elements[i]);
      }
      os << html::tr(elements);
    }
    os << "</table>";
  }
  
  void HTMLWriter::write(ProfdataCompare &comparer) {
    sys::fs::create_directories(dirname);
    writeCSS();
    writeSummary(comparer);
    for (auto &comparison : comparer.comparisons) {
      writeComparisonReport(*comparison);
    }
  }
  
  void HTMLWriter::writeComparisonReport(FileComparison &comparison) {
    auto newName = dirname + "/" + comparison.newItem->name + ".html";
    error_code error;
    raw_fd_ostream os(newName,
                      error, sys::fs::F_RW);
    if (error) exitWithErrorCode(error);
    wrapHTMLOutput(os, comparison.newItem->name, [this, &comparison, &os] {
      Column functionCol("Function");
      Column oldCovCol("Previous Coverage", Column::Alignment::Center);
      Column newCovCol("Current Coverage", Column::Alignment::Center);
      Column diffCol("Coverage Difference", Column::Alignment::Center);
      for (auto &funcComparison : comparison.functionComparisons()) {
        double newCoverage = funcComparison.newItem->coveragePercentage();
        string newCovString = formattedDouble(newCoverage);
        string oldCovString = "N/A";
        string diffString = "N/A";
        string _class = "";
        if (auto func = funcComparison.oldItem) {
          double oldCoverage = func->coveragePercentage();
          oldCovString = formattedDouble(oldCoverage);
          diffString = funcComparison.formattedCoverageDifference();
          
          CoverageStatus status =
            statusForDiff(funcComparison.coverageDifference());
          _class = coverageStatusString(status);
        }
        string symbol = funcComparison.functionName();
        functionCol.add(html::escape(symbol));
        oldCovCol.add(oldCovString);
        newCovCol.add(newCovString);
        diffCol.add(html::span(_class, diffString));
      }
      this->writeTable({ functionCol, oldCovCol, newCovCol, diffCol }, os);
    });
  }
  
  void HTMLWriter::writeSummary(ProfdataCompare &comparer) {
    error_code error;
    raw_fd_ostream os(dirname + "/index.html", error, sys::fs::F_RW);
    if (error) exitWithErrorCode(error);
    
    string oldFn = sys::path::filename(comparer.oldFile);
    string newFn = sys::path::filename(comparer.newFile);
    auto title = oldFn + " vs. " + newFn;
    
    wrapHTMLOutput(os, title, [this, &comparer, &os] {
      Column fnCol("Filename");
      Column prevCol("Previous Coverage", Column::Alignment::Center);
      Column currCol("Current Coverage", Column::Alignment::Center);
      Column diffCol("Coverage Difference", Column::Alignment::Center);
      for (auto &cmp : comparer.comparisons) {
        
        string oldPercentage = cmp->oldItem ?
          formattedDouble(cmp->oldItem->coveragePercentage()) : "N/A";
        
        string newPercentage =
          formattedDouble(cmp->newItem->coveragePercentage());
        
        auto fn = html::escape(cmp->newItem->name);
        CoverageStatus status = statusForDiff(cmp->coverageDifference());
        string _class = coverageStatusString(status);
        fnCol.add(html::a(fn + ".html", fn));
        prevCol.add(oldPercentage);
        currCol.add(newPercentage);
        diffCol.add(html::span(_class,
                        html::escape(cmp->formattedCoverageDifference())));
      }
      this->writeTable({ fnCol, prevCol, currCol, diffCol }, os);
    });
  }
  
  void HTMLWriter::writeCSS() {
    string css = "body {"
                 "  font-family: -apple-system, sans-serif;"
                 "}"
                 "footer {"
                 "  padding: 15px;"
                 "}"
                 "a {"
                 "  text-decoration: none;"
                 "}"
                 "table, th, td {"
                 "  padding: 10px;"
                 "  text-align: left;"
                 "  border: 1px solid #ddd;"
                 "  border-collapse: collapse;"
                 "}"
                 ".warning {"
                 "  color: #f9a03f;"
                 "  font-weight: normal;"
                 "}"
                 ".bad {"
                 "  color: #ba1b1d;"
                 "  font-weight: bold;"
                 "}"
                 ".good {"
                 "  color: #1cc000;"
                 "  font-weight: normal;"
                 "}";
    error_code error;
    raw_fd_ostream out(dirname + "/style.css", error, sys::fs::F_RW);
    if (error) exitWithErrorCode(error);
    out << css;
  }
  
  void HTMLWriter::wrapHTMLOutput(raw_ostream &out,
                                  string title,
                                  HTMLOutputFunction innerGen) {
    out << "<!DOCTYPE html><html><head>" <<
           html::tag("title", title) <<
           "<meta name='viewport'"
           "content='width=device-width, initial-scale=1'>";
    out << "<link rel='stylesheet' href='style.css' type='text/css'>";
    out << "</head><body>";
    innerGen();
    
    auto end = chrono::system_clock::now();
    auto end_time = chrono::system_clock::to_time_t(end);
    auto date_str = ctime(&end_time);
    out << html::tag("footer",
                     "Generated by cov-compare on " + html::escape(date_str));
    out << "</body></html>";
  }
}
