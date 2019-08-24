// RUN: not %target-swift-frontend %s -typecheck

_ = [1].reduce( [Int:Int]() ) {
  (dict, num) in dict[num] = num * num
}
