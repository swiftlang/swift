//===--- GenLValue.h - Swift IR generation for l-values ---------*- C++ -*-===//
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
//  This file provides the private interface to the l-value emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENLVALUE_H
#define SWIFT_IRGEN_GENLVALUE_H

namespace swift {
  class MemberRefExpr;
  class RequalifyExpr;
  class SubscriptExpr;

namespace irgen {
  class Explosion;
  class IRGenFunction;
  class LValue;

  /// Emit a requalification expression as an r-value.
  void emitRequalify(IRGenFunction &IGF, RequalifyExpr *E, Explosion &expl);

  /// Emit an l-value for a member reference.
  LValue emitMemberRefLValue(IRGenFunction &IGF, MemberRefExpr *E);

  /// Emit an l-value for a subscripting.
  LValue emitSubscriptLValue(IRGenFunction &IGF, SubscriptExpr *E);

} // end namespace irgen
} // end namespace swift

#endif
