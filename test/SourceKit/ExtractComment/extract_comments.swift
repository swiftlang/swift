// RUN: %sourcekitd-test -req=extract-comment -pass-as-sourcetext %S/Inputs/DocComment1.swift > %t.DocComment1.response
// RUN: diff -u %s.DocComment1.response %t.DocComment1.response

// RUN: %sourcekitd-test -req=extract-comment -pass-as-sourcetext %S/Inputs/DocComment2.swift > %t.DocComment2.response
// RUN: diff -u %s.DocComment2.response %t.DocComment2.response

// RUN: %sourcekitd-test -req=extract-comment -pass-as-sourcetext %S/Inputs/DocComment3.swift > %t.DocComment3.response
// RUN: diff -u %s.DocComment3.response %t.DocComment3.response

// RUN: %sourcekitd-test -req=extract-comment -pass-as-sourcetext %S/Inputs/DocCommentEmptyLine1.swift > %t.DocCommentEmptyLine1.response
// RUN: diff -u %s.DocCommentEmptyLine1.response %t.DocCommentEmptyLine1.response

// RUN: %sourcekitd-test -req=extract-comment -pass-as-sourcetext %S/Inputs/DocCommentEmptyLine2.swift > %t.DocCommentEmptyLine2.response
// RUN: diff -u %s.DocCommentEmptyLine2.response %t.DocCommentEmptyLine2.response

// RUN: %sourcekitd-test -req=extract-comment -pass-as-sourcetext %S/Inputs/DocCommentEmptyLine3.swift > %t.DocCommentEmptyLine3.response
// RUN: diff -u %s.DocCommentEmptyLine3.response %t.DocCommentEmptyLine3.response

// RUN: %sourcekitd-test -req=extract-comment -pass-as-sourcetext %S/Inputs/Comment1.swift > %t.Comment1.response
// RUN: diff -u %s.Comment1.response %t.Comment1.response

// RUN: %sourcekitd-test -req=extract-comment -pass-as-sourcetext %S/Inputs/CommentIndent1.swift > %t.CommentIndent1.response
// RUN: diff -u %s.CommentIndent1.response %t.CommentIndent1.response

// RUN: %sourcekitd-test -req=extract-comment -pass-as-sourcetext %S/Inputs/CommentIndent2.swift > %t.CommentIndent2.response
// RUN: diff -u %s.CommentIndent2.response %t.CommentIndent2.response

// RUN: %sourcekitd-test -req=extract-comment -pass-as-sourcetext %S/Inputs/NotComment1.swift
