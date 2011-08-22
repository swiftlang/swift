//===--- ParseResult.h - ParseResult Helper Class ---------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef PARSERESULT_H
#define PARSERESULT_H

#include "llvm/ADT/NullablePtr.h"

namespace swift {
  
/// ParseResult - This is used for code in the parser and Sema that returns an
/// AST node that can fail for one of two reasons: 1) a parse error, or
/// 2) a semantic error.  In the later case, we should continue parsing but
/// ignore the parsed node.  In the former case, we should abort parsing and try
/// recovery.  This also allows direct representation of Absent, which is when
/// the parser didn't try parsing this node for some reason.
///
/// For compatibility with 'true returns mean parse failure', this evaluates as
/// true when a parse failure in a bool context.
///
/// Wouldn't it be nice to express this as a oneof?
///
template<typename T>
class ParseResult {
  T *Value;
public:
  ParseResult() : Value(0) {} // Initialize to Absent.
  ParseResult(T *V) { operator=(V); }
  ParseResult(bool V) {
    assert(V && "Should only be used for 'return true'");
    Value = (T*)1;
  }
  
  /// When constructed from a NullablePtr, we know that this is a Sema result,
  /// so we either have a valid value or a SemaError.
  ParseResult(llvm::NullablePtr<T> Arg) {
    if (Arg.isNull())
      Value = (T*)2;
    else
      Value = Arg.get();
  }
  
  template<typename T2>
  ParseResult(ParseResult<T2> V) {
    if (!V.isSuccess())     // Sentinels all have same representation.
      Value = (T*)V.getSentinelValue();
    else
      Value = V.get();      // Let C++ compiler check type compatibility.
  }
  
  static ParseResult getSemaError() {
    return ParseResult(llvm::NullablePtr<T>());
  }
  
  bool isAbsent() const { return Value == 0; }
  bool isParseError() const { return Value == (T*)1; }
  bool isSemaError() const { return Value == (T*)2; }
  bool isSuccess() const {
    return !isAbsent() && !isParseError() && !isSemaError();
  }
  
  operator bool() const { return isParseError(); }
  
  void operator=(T *V) {
    Value = V;
    assert(isSuccess() && "Didn't assign a normal value");
  }
  
  T *get() const {
    assert(isSuccess() && "Not a successful parse");
    return Value;
  }
  
  T *getSentinelValue() const { 
    assert(!isSuccess() && "Not a sentinel");
    return Value;
  }
};
  
} // end namespace swift

#endif

