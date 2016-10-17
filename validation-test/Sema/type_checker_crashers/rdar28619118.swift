// RUN: not --crash %target-swift-frontend %s -parse

_ = [1].reduce( [Int:Int]() ) {
  (dict, num) in dict[num] = num * num
}
