//===--- SILExternalSource.h - On-demand generation of SIL ------*- C++ -*-===//
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
//
// This file defines the abstract SILExternalSource class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILEXTERNALSOURCE_H
#define SWIFT_SILEXTERNALSOURCE_H

namespace swift {

class SILFunction;

class SILExternalSource {
public:
  SILExternalSource() { }
  virtual ~SILExternalSource() = default;

  /// SILExternalSource gets called for each external function
  /// that the SIL linker would try to load SIL for.  In particular
  /// this means transparent functions.
  ///
  /// \param callee is the (usually empty) called function.
  virtual SILFunction *lookupSILFunction(SILFunction *callee) = 0;

private:
  virtual void anchor();
};

} // namespace swift

#endif
