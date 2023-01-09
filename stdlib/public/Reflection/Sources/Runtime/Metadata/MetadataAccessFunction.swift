//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift

extension Metadata {
  @frozen
  public struct AccessFunction {
    @usableFromInline
    let ptr: UnsafeRawPointer
    
    @inlinable
    init(_ ptr: UnsafeRawPointer) {
      self.ptr = ptr
    }
    
    // MARK: Access Function 0 Args
    
    @usableFromInline
    typealias AccessFn0 = @convention(thin) (
      Request
    ) -> Response
    
    @inlinable
    public func callAsFunction(_ request: Request) -> Metadata {
      let fn = unsafeBitCast(ptr, to: AccessFn0.self)
      
      return fn(request).metadata
    }
    
    // MARK: Access Function 1 Arg
    
    @usableFromInline
    typealias AccessFn1 = @convention(thin) (
      Request,
      Metadata
    ) -> Response
    
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ arg0: Metadata
    ) -> Metadata {
      let fn = unsafeBitCast(ptr, to: AccessFn1.self)
      
      return fn(request, arg0).metadata
    }
    
    // MARK: Access Function 2 Args
    
    @usableFromInline
    typealias AccessFn2 = @convention(thin) (
      Request,
      Metadata,
      Metadata
    ) -> Response
    
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ arg0: Metadata,
      _ arg1: Metadata
    ) -> Metadata {
      let fn = unsafeBitCast(ptr, to: AccessFn2.self)
      
      return fn(request, arg0, arg1).metadata
    }
    
    // MARK: Access Function 3 Args
    
    @usableFromInline
    typealias AccessFn3 = @convention(thin) (
      Request,
      Metadata,
      Metadata,
      Metadata
    ) -> Response
    
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ arg0: Metadata,
      _ arg1: Metadata,
      _ arg2: Metadata
    ) -> Metadata {
      let fn = unsafeBitCast(ptr, to: AccessFn3.self)
      
      return fn(request, arg0, arg1, arg2).metadata
    }
    
    // MARK: Access Function Many Args
    
    @usableFromInline
    typealias AccessFnMany = @convention(thin) (
      Request,
      UnsafePointer<Metadata>
    ) -> Response
    
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ args: [Metadata]
    ) -> Metadata {
      let fn = unsafeBitCast(ptr, to: AccessFnMany.self)
      
      return args.withUnsafeBufferPointer {
        fn(request, $0.baseAddress.unsafelyUnwrapped).metadata
      }
    }
  }
}
