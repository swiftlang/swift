// RUN: %utils/gyb --line-directive '' %S/Inputs/TokenKindList.txt.gyb | sort > %T/python_kinds.txt
// RUN: %swift-syntax-test --dump-all-syntax-tokens | sort > %T/def_kinds.txt
// RUN: diff %T/def_kinds.txt %T/python_kinds.txt

// Check that all token kinds listed in TokenKinds.def are also in
// gyb_syntax_support/Token.py