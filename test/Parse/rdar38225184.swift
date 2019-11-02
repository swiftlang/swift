// RUN: %target-typecheck-verify-swift

extension Collection where Element == Int && Index == Int {}
// expected-error@-1 {{expected ',' to separate the requirements of this 'where' clause}} {{43-45=,}}
