// RUN: %target-swift-frontend -primary-file %s -emit-ir  -disable-availability-checking -g | %FileCheck %s
// REQUIRES: concurrency

// Don't assert on dynamically sized variables.
// CHECK: define{{( dllexport)?}}{{( protected)?}} swift{{(tail)?}}cc void @"$s5debug1fyxxYaKlF"

public func f<Success>(_ value: Success) async throws -> Success {
  switch Result<Success, Error>.success(value) {
    case .success(let success):
    return success

  case .failure(let error):
    throw error;
  }
}
