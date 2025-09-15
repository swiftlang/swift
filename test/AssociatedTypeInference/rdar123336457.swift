// RUN: %target-typecheck-verify-swift

protocol Task1: AnyObject {
}

protocol Task2: AnyObject {
}

class TaskImpl : Task2 {
}

protocol TaskResult {
  associatedtype Task

  var tasks: [Task] { get }

  func removeMatchedTask(_ task: Task)
  func match(_ task: Task) -> Bool
}

extension TaskResult where Task == Task1 {
  func match(_ task: Task) -> Bool { false }
}

extension TaskResult where Task: Task2 {
  func match(_ task: Task) -> Bool { false }
}

final class Results: TaskResult {
  var tasks: [TaskImpl] = []

  func removeMatchedTask(_ task: TaskImpl) {}
}
