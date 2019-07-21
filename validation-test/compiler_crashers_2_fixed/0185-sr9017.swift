// RUN: not %target-swift-frontend -emit-ir %s

public final class Action<Input, Error: Swift.Error> {

extension Action {

public enum ActionError<Error: Swift.Error>: Swift.Error {
  case disabled
