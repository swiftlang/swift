//===--- Result.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

public enum Result<Value> {
case Success(Value)
case Error(Error)

  init(success x: Value) {
    self = .Success(x)
  }
  
  init(error: Error) {
    self = .Error(error)
  }
  
  func map<U>(_ transform: (Value) -> U) -> Result<U> {
    switch self {
    case .Success(let x): return .Success(transform(x))
    case .Error(let e): return .Error(e)
    }
  }

  func flatMap<U>(_ transform: (Value) -> Result<U>) -> Result<U> {
    switch self {
    case .Success(let x): return transform(x)
    case .Error(let e): return .Error(e)
    }
  }

  func get() throws -> Value {
    switch self {
    case .Success(let x): return x
    case .Error(let e): throw e
    }
  }

  var success: Value? {
    switch self {
    case .Success(let x): return x
    case .Error: return nil
    }
  }

  var error: Error? {
    switch self {
    case .Success: return nil
    case .Error(let x): return x
    }
  }
}

public func ?? <T> (
  result: Result<T>, defaultValue: @autoclosure () -> T
) -> T {
  switch result {
  case .Success(let x): return x
  case .Error: return defaultValue()
  }
}

// We aren't actually proposing this overload; we think there should
// be a compiler warning that catches the promotion that you probably
// don't want.
public func ?? <T> (
  result: Result<T>?, defaultValue: @autoclosure () -> T
) -> T {
  fatalError("We should warn about Result<T> being promoted to Result<T>?")
}

/// Translate the execution of a throwing closure into a Result
func catchResult<Success>(
  invoking body: () throws -> Success
) -> Result<Success> {
  do {
    return try .Success(body())
  }
  catch {
    return .Error(error)
  }
}

// A couple of error types
enum Nasty : Error {
case Bad, Awful, Terrible
}

enum Icky : Error {
case Sad, Bad, Poor
}

// Some Results to work with
let three = Result(success: 3)
let four = Result(success: 4)
let nasty = Result<Int>(error: Nasty.Bad)
let icky = Result<String>(error: Icky.Sad)

print(three)
print(nasty)
print(icky)

print(three ?? 4)
print(nasty ?? 4)

print(three.map { String($0) })
print(nasty.map { String($0) })


print(three.flatMap { .Success(String($0)) })
print(nasty.flatMap { .Success(String($0)) })

print(three.flatMap { _ in icky })
print(nasty.flatMap { _ in icky })

try print(three.get())
do {
  try print(nasty.get())
}
catch {
  print(error)
}

func mayFail(_ fail: Bool) throws -> Int {
  if fail { throw Icky.Poor }
  return 0
}

print(catchResult { try mayFail(true) })
print(catchResult { try mayFail(false) })

print(catchResult { _ in 1 }.flatMap { _ in Result(success: 4) }.flatMap { _ in Result<String>(error: Icky.Poor) })
print(catchResult { _ in 1 }.map { _ in three }.flatMap {$0} )

let results = [three, nasty, four]
print(results.flatMap { $0.success })
print(results.flatMap { $0.error })
print(results.contains { $0.success != nil })

// Mistaken usage; causes Result<T> to be promoted to Result<T>?
// print(three ?? nasty)
