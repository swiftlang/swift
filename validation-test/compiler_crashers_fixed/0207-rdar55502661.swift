// RUN: not %target-swift-frontend %s -typecheck -o /dev/null

extension Result {
  extension Result where Result.Undefined == Int {
