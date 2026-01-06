//===--- IfConfigClauseRangeInfo.h - header for #if clauses -=====---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_IFCONFIGCLAUSERANGEINFO_H
#define SWIFT_AST_IFCONFIGCLAUSERANGEINFO_H

#include "swift/Basic/SourceLoc.h"

namespace swift {

class SourceManager;

/// Stores range information for a \c #if block in a SourceFile.
class IfConfigClauseRangeInfo final {
public:
  enum ClauseKind {
    // Active '#if', '#elseif', or '#else' clause.
    ActiveClause,
    // Inactive '#if', '#elseif', or '#else' clause.
    InactiveClause,
    // '#endif' directive.
    EndDirective,
  };

private:
  /// Source location of '#if', '#elseif', etc.
  SourceLoc DirectiveLoc;
  /// Character source location of body starts.
  SourceLoc BodyLoc;
  /// Location of the end of the body.
  SourceLoc EndLoc;

  ClauseKind Kind;

public:
  IfConfigClauseRangeInfo(SourceLoc DirectiveLoc, SourceLoc BodyLoc,
                          SourceLoc EndLoc, ClauseKind Kind)
      : DirectiveLoc(DirectiveLoc), BodyLoc(BodyLoc), EndLoc(EndLoc),
        Kind(Kind) {
    assert(DirectiveLoc.isValid() && BodyLoc.isValid() && EndLoc.isValid());
  }

  SourceLoc getStartLoc() const { return DirectiveLoc; }
  CharSourceRange getDirectiveRange(const SourceManager &SM) const;
  CharSourceRange getWholeRange(const SourceManager &SM) const;
  CharSourceRange getBodyRange(const SourceManager &SM) const;

  ClauseKind getKind() const { return Kind; }
};

}

#endif /* SWIFT_AST_IFCONFIGCLAUSERANGEINFO_H */
