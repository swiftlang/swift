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

@available(SwiftStdlib 5.9, *)
extension Metadata {
  @available(SwiftStdlib 5.9, *)
  @frozen
  public struct AccessFunction {
    @usableFromInline
    let ptr: UnsafeRawPointer

    @available(SwiftStdlib 5.9, *)
    @inlinable
    init(_ ptr: UnsafeRawPointer) {
      self.ptr = ptr
    }

//===----------------------------------------------------------------------===//
// 0 Arguments
//===----------------------------------------------------------------------===//

    @usableFromInline
    typealias AccessFn0 = @convention(thin) (
      Request
    ) -> Response

    @available(SwiftStdlib 5.9, *)
    @inlinable
    public func callAsFunction(_ request: Request) -> Metadata {
      let signedPtr = PtrAuth.signAccessFn0(ptr)
      let fn = unsafeBitCast(signedPtr, to: AccessFn0.self)

      return fn(request).metadata
    }

//===----------------------------------------------------------------------===//
// 1 Argument
//===----------------------------------------------------------------------===//

    @usableFromInline
    typealias AccessFn1 = @convention(thin) (
      Request,
      UnsafeRawPointer
    ) -> Response

    @available(SwiftStdlib 5.9, *)
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ arg0: any Any.Type
    ) -> Metadata {
      self(request, Metadata(arg0))
    }

    @available(SwiftStdlib 5.9, *)
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ arg0: Metadata
    ) -> Metadata {
      self(request, arg0.ptr)
    }

    @available(SwiftStdlib 5.9, *)
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ arg0: UnsafeRawPointer
    ) -> Metadata {
      let signedPtr = PtrAuth.signAccessFn1(ptr)
      let fn = unsafeBitCast(signedPtr, to: AccessFn1.self)

      return fn(request, arg0).metadata
    }

//===----------------------------------------------------------------------===//
// 2 Arguments
//===----------------------------------------------------------------------===//

    @usableFromInline
    typealias AccessFn2 = @convention(thin) (
      Request,
      UnsafeRawPointer,
      UnsafeRawPointer
    ) -> Response

    @available(SwiftStdlib 5.9, *)
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ arg0: any Any.Type,
      _ arg1: any Any.Type
    ) -> Metadata {
      self(request, Metadata(arg0), Metadata(arg1))
    }

    @available(SwiftStdlib 5.9, *)
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ arg0: Metadata,
      _ arg1: Metadata
    ) -> Metadata {
      self(request, arg0.ptr, arg1.ptr)
    }

    @available(SwiftStdlib 5.9, *)
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ arg0: UnsafeRawPointer,
      _ arg1: UnsafeRawPointer
    ) -> Metadata {
      let signedPtr = PtrAuth.signAccessFn2(ptr)
      let fn = unsafeBitCast(signedPtr, to: AccessFn2.self)

      return fn(request, arg0, arg1).metadata
    }

//===----------------------------------------------------------------------===//
// 3 Arguments
//===----------------------------------------------------------------------===//

    @usableFromInline
    typealias AccessFn3 = @convention(thin) (
      Request,
      UnsafeRawPointer,
      UnsafeRawPointer,
      UnsafeRawPointer
    ) -> Response

    @available(SwiftStdlib 5.9, *)
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ arg0: any Any.Type,
      _ arg1: any Any.Type,
      _ arg2: any Any.Type
    ) -> Metadata {
      self(request, Metadata(arg0), Metadata(arg1), Metadata(arg2))
    }

    @available(SwiftStdlib 5.9, *)
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ arg0: Metadata,
      _ arg1: Metadata,
      _ arg2: Metadata
    ) -> Metadata {
      self(request, arg0.ptr, arg1.ptr, arg2.ptr)
    }

    @available(SwiftStdlib 5.9, *)
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ arg0: UnsafeRawPointer,
      _ arg1: UnsafeRawPointer,
      _ arg2: UnsafeRawPointer
    ) -> Metadata {
      let signedPtr = PtrAuth.signAccessFn3(ptr)
      let fn = unsafeBitCast(signedPtr, to: AccessFn3.self)

      return fn(request, arg0, arg1, arg2).metadata
    }

//===----------------------------------------------------------------------===//
// Many Arguments
//===----------------------------------------------------------------------===//
    
    @usableFromInline
    typealias AccessFnMany = @convention(thin) (
      Request,
      UnsafePointer<UnsafeRawPointer>
    ) -> Response

    @available(SwiftStdlib 5.9, *)
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ args: [any Any.Type]
    ) -> Metadata {
      let signedPtr = PtrAuth.signAccessFnMany(ptr)
      let fn = unsafeBitCast(signedPtr, to: AccessFnMany.self)

      return args.withUnsafeBufferPointer {
        fn(
          request,
          UnsafePointer<UnsafeRawPointer>(
            $0.baseAddress.unsafelyUnwrapped._rawValue
          )
        ).metadata
      }
    }

    @available(SwiftStdlib 5.9, *)
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ args: [Metadata]
    ) -> Metadata {
      let signedPtr = PtrAuth.signAccessFnMany(ptr)
      let fn = unsafeBitCast(signedPtr, to: AccessFnMany.self)
      
      return args.withUnsafeBufferPointer {
        fn(
          request,
          UnsafePointer<UnsafeRawPointer>(
            $0.baseAddress.unsafelyUnwrapped._rawValue
          )
        ).metadata
      }
    }

    @available(SwiftStdlib 5.9, *)
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ args: [UnsafeRawPointer]
    ) -> Metadata {
      args.withUnsafeBufferPointer {
        self(request, $0)
      }
    }

    @available(SwiftStdlib 5.9, *)
    @inlinable
    public func callAsFunction(
      _ request: Request,
      _ args: UnsafeBufferPointer<UnsafeRawPointer>
    ) -> Metadata {
      let signedPtr = PtrAuth.signAccessFnMany(ptr)
      let fn = unsafeBitCast(signedPtr, to: AccessFnMany.self)

      return fn(request, args.baseAddress.unsafelyUnwrapped).metadata
    }
  }
}
