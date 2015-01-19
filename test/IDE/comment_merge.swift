// NOTE: This file is sensitive to line numbers.  Thus RUN and CHECK lines come
// below the code.
//
// NOTE: Please don't change this file to use FileCheck's feature to match
// relative line numbers: those lines are comments and we don't want to see
// anything extra in a test for documentation comments.




//===--- Check how we merge consecutive comments.

// not_doc1  NOT_DOC
func not_doc1() {}

/* not_doc2  NOT_DOC */
func not_doc2() {}

//! Doxygen comment, but not Swift.  not_doc3  NOT_DOC
func not_doc3() {}

/*! Doxygen comment, but not Swift.  not_doc4  NOT_DOC */
func not_doc4() {}

/*/ not_doc5  NOT_DOC */
func not_doc5() {}

/** is_doc6 IS_DOC_SINGLE */
func is_doc6() {}

/**
 * is_doc7  IS_DOC_SINGLE */
func is_doc7() {}

/**
 * is_doc8
 * IS_DOC_SINGLE */
func is_doc8() {}

/**
 * is_doc9
 * IS_DOC_SINGLE
 */
func is_doc9() {}

/// is_doc10 IS_DOC_SINGLE
func is_doc10() {}

// Aaa.  is_doc11 NOT_DOC
/// Bbb.  is_doc11 IS_DOC_SINGLE
func is_doc11() {}

/// This comment is attached in Doxygen, but not in Swift.  not_doc12 IS_DOC_NOT_ATTACHED
// Not a Doxygen comment.  NOT_DOC
func not_doc12() {}

/** This comment is attached in Doxygen, but not in Swift.  not_doc13 IS_DOC_NOT_ATTACHED */
/* Not a Doxygen comment.  not_doc13  NOT_DOC */
func not_doc13() {}

/// is_doc14 IS_DOC_START
/// IS_DOC_END
func is_doc14() {}

/// is_doc15 IS_DOC_START
/// Aaa bbb ccc.
/// IS_DOC_END
func is_doc15() {}

/// is_doc16 IS_DOC_START
/** Aaa bbb ccc. */
/// IS_DOC_END
func is_doc16() {}

/** Aaa is_doc17 IS_DOC_START *//** bbb */
/// IS_DOC_END
func is_doc17() {}

/// IS_DOC_NOT_ATTACHED
// NOT_DOC
/// is_doc18 IS_DOC_START IS_DOC_END
func is_doc18() {}

// Aaa.  NOT_DOC
/// is_doc19 IS_DOC_START
/** Aaa
 *
 * Bbb */
/// Ccc
/** Ddd
 *  Eee  IS_DOC_END
 */
func is_doc19() {}

// RUN: %target-swift-ide-test -print-comments -source-filename %s > %t.txt
// RUN: FileCheck %s -check-prefix=WRONG < %t.txt
// RUN: FileCheck %s < %t.txt

// Non-documentation comments should not be attached to anything.
// WRONG-NOT: NOT_DOC

// Some comments are not attached to anything.
// WRONG-NOT: IS_DOC_NOT_ATTACHED

// Ensure we don't pick up extra comments.
// WRONG-NOT: RawComment={{.*}}IS_DOC_START{{.*}}IS_DOC_START{{.*}}BriefComment=
// WRONG-NOT: RawComment={{.*}}IS_DOC_END{{.*}}IS_DOC_END{{.*}}BriefComment=

// CHECK: comment_merge.swift:14:6: Func/not_doc1 RawComment=none
// CHECK-NEXT: comment_merge.swift:17:6: Func/not_doc2 RawComment=none
// CHECK-NEXT: comment_merge.swift:20:6: Func/not_doc3 RawComment=none
// CHECK-NEXT: comment_merge.swift:23:6: Func/not_doc4 RawComment=none
// CHECK-NEXT: comment_merge.swift:26:6: Func/not_doc5 RawComment=none
// CHECK-NEXT: comment_merge.swift:29:6: Func/is_doc6 RawComment=[/** is_doc6 IS_DOC_SINGLE */]
// CHECK-NEXT: comment_merge.swift:33:6: Func/is_doc7 RawComment=[/**\n * is_doc7  IS_DOC_SINGLE */]
// CHECK-NEXT: comment_merge.swift:38:6: Func/is_doc8 RawComment=[/**\n * is_doc8\n * IS_DOC_SINGLE */]
// CHECK-NEXT: comment_merge.swift:44:6: Func/is_doc9 RawComment=[/**\n * is_doc9\n * IS_DOC_SINGLE\n */]
// CHECK-NEXT: comment_merge.swift:47:6: Func/is_doc10 RawComment=[/// is_doc10 IS_DOC_SINGLE\n]
// CHECK-NEXT: comment_merge.swift:51:6: Func/is_doc11 RawComment=[/// Bbb.  is_doc11 IS_DOC_SINGLE\n]
// CHECK-NEXT: comment_merge.swift:55:6: Func/not_doc12 RawComment=none
// CHECK-NEXT: comment_merge.swift:59:6: Func/not_doc13 RawComment=none
// CHECK-NEXT: comment_merge.swift:63:6: Func/is_doc14 RawComment=[/// is_doc14 IS_DOC_START\n/// IS_DOC_END\n]
// CHECK-NEXT: comment_merge.swift:68:6: Func/is_doc15 RawComment=[/// is_doc15 IS_DOC_START\n/// Aaa bbb ccc.\n/// IS_DOC_END\n]
// CHECK-NEXT: comment_merge.swift:73:6: Func/is_doc16 RawComment=[/// is_doc16 IS_DOC_START\n/** Aaa bbb ccc. *//// IS_DOC_END\n]
// CHECK-NEXT: comment_merge.swift:77:6: Func/is_doc17 RawComment=[/** Aaa is_doc17 IS_DOC_START *//** bbb *//// IS_DOC_END\n]
// CHECK-NEXT: comment_merge.swift:82:6: Func/is_doc18 RawComment=[/// is_doc18 IS_DOC_START IS_DOC_END\n]
// CHECK-NEXT: comment_merge.swift:93:6: Func/is_doc19 RawComment=[/// is_doc19 IS_DOC_START\n/** Aaa\n *\n * Bbb *//// Ccc\n/** Ddd\n *  Eee  IS_DOC_END\n */]

