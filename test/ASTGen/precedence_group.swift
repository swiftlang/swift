// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

precedencegroup AssignsTrue { assignment: true }
precedencegroup AssignsFalse { assignment: false }
precedencegroup LeftAssoc { associativity: left }
precedencegroup NoAssoc { associativity: none }

precedencegroup BadAssignment { assignment: maybe }
// expected-error@-1 {{expected 'true' or 'false' after 'assignment'}}
precedencegroup BadAssoc { associativity: sideways }
// expected-error@-1 {{Expected 'none', 'left', or 'right' after 'associativity'}}
