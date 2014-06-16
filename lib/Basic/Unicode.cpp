//===--- Unicode.cpp - Unicode utilities ----------------------------------===//
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

#include "swift/Basic/Unicode.h"
#include "llvm/Support/ConvertUTF.h"

using namespace swift;

StringRef swift::unicode::extractFirstExtendedGraphemeCluster(StringRef S) {
  // Extended grapheme cluster segmentation algorithm as described in Unicode
  // Standard Annex #29.
  if (S.empty())
    return StringRef();

  const UTF8 *SourceStart = reinterpret_cast<const UTF8 *>(S.data());

  const UTF8 *SourceNext = SourceStart;
  UTF32 C[2];
  UTF32 *TargetStart = C;

  ConvertUTF8toUTF32(&SourceNext, SourceStart + S.size(), &TargetStart, C + 1,
                     lenientConversion);
  if (TargetStart == C) {
    // The source string contains an ill-formed subsequence at the end.
    return S;
  }

  GraphemeClusterBreakProperty GCBForC0 = getGraphemeClusterBreakProperty(C[0]);
  while (true) {
    if (isExtendedGraphemeClusterBoundaryAfter(GCBForC0))
      return S.slice(0, SourceNext - SourceStart);

    size_t C1Offset = SourceNext - SourceStart;
    ConvertUTF8toUTF32(&SourceNext, SourceStart + S.size(), &TargetStart, C + 2,
                       lenientConversion);

    if (TargetStart == C + 1) {
      // End of source string or the source string contains an ill-formed
      // subsequence at the end.
      return S.slice(0, C1Offset);
    }

    GraphemeClusterBreakProperty GCBForC1 =
        getGraphemeClusterBreakProperty(C[1]);
    if (isExtendedGraphemeClusterBoundary(GCBForC0, GCBForC1))
      return S.slice(0, C1Offset);

    C[0] = C[1];
    TargetStart = C + 1;
    GCBForC0 = GCBForC1;
  }
}

