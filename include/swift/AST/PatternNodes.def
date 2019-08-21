//===--- PatternNodes.def - Swift Pattern AST Metaprogramming ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines macros used for macro-metaprogramming with patterns.
//
//===----------------------------------------------------------------------===//

/// PATTERN(Id, Parent)
///   The pattern's enumerator value is PatternKind::Id.  The pattern's
///   class name is Id##Pattern, and the name of its base class is Parent.
#ifndef PATTERN
# error Included PatternNodes.def without defining PATTERN!
#endif

/// REFUTABLE_PATTERN(Id, Parent)
///   Matching this pattern can fail. These patterns cannot appear syntactically
///   in variable declarations, assignments, or function declarations.
#ifndef REFUTABLE_PATTERN
# define REFUTABLE_PATTERN(Id, Parent) PATTERN(Id, Parent)
#endif

#ifndef LAST_PATTERN
#define LAST_PATTERN(Id)
#endif

// Metavars: x (variable), pat (pattern), e (expression)
PATTERN(Paren, Pattern) // (pat)
PATTERN(Tuple, Pattern) // (pat1, ..., patN), N >= 1
PATTERN(Named, Pattern) // let pat, var pat
PATTERN(Any, Pattern)   // _
PATTERN(Typed, Pattern) // pat : type
PATTERN(Var, Pattern)   // x
REFUTABLE_PATTERN(Is, Pattern)           // x is myclass
REFUTABLE_PATTERN(EnumElement, Pattern)  // .mycase(pat1, ..., patN)
                                         // MyType.mycase(pat1, ..., patN)
REFUTABLE_PATTERN(OptionalSome, Pattern) // pat?  nil
REFUTABLE_PATTERN(Bool, Pattern)         // true, false
REFUTABLE_PATTERN(Expr, Pattern)         // e
LAST_PATTERN(Expr)

#undef REFUTABLE_PATTERN
#undef PATTERN
#undef LAST_PATTERN
