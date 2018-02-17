//===--------------- SourceKitdRequest.swift ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// This file provides a convenient way to build a sourcekitd request.
//===----------------------------------------------------------------------===//

import sourcekitd

public struct SourceKitdRequest: CustomStringConvertible {

  public class Dictionary: CustomStringConvertible {
    let dict: sourcekitd_object_t

    public init() {
      dict = sourcekitd_request_dictionary_create(nil, nil, 0)
    }

    deinit {
      sourcekitd_request_release(UnsafeMutableRawPointer(dict))
    }

    public func add(_ key: SourceKitdUID, value: String) {
      sourcekitd_request_dictionary_set_string(dict, key.uid, value)
    }

    public func add(_ key: SourceKitdUID, value: Int) {
      sourcekitd_request_dictionary_set_int64(dict, key.uid, Int64(value))
    }

    public func add(_ key: SourceKitdUID, value: SourceKitdUID) {
      sourcekitd_request_dictionary_set_uid(dict, key.uid, value.uid)
    }

    public func add(_ key: SourceKitdUID, value: Array) {
      sourcekitd_request_dictionary_set_value(dict, key.uid, value.arr)
    }

    public func add(_ key: SourceKitdUID, value: Dictionary) {
      sourcekitd_request_dictionary_set_value(dict, key.uid, value.dict)
    }

    public func add(_ key: SourceKitdUID, value: Bool) {
      sourcekitd_request_dictionary_set_int64(dict, key.uid, value ? 1 : 0)
    }

    public var description: String {
      let utf8Str = sourcekitd_request_description_copy(dict)!
      let result = String(cString: utf8Str)
      free(utf8Str)
      return result
    }

  }

  public class Array: CustomStringConvertible {
    let arr: sourcekitd_object_t
    private let Append: Int = -1

    public init() {
      arr = sourcekitd_request_array_create(nil, 0)
    }

    deinit {
      sourcekitd_request_release(arr)
    }

    public func add(_ value: String) {
      sourcekitd_request_array_set_string(arr, Append, value)
    }

    public func add(_ value: Int) {
      sourcekitd_request_array_set_int64(arr, Append, Int64(value))
    }

    public func add(_ value: SourceKitdUID) {
      sourcekitd_request_array_set_uid(arr, Append, value.uid)
    }

    public func add(_ value: Dictionary) {
      sourcekitd_request_array_set_value(arr, Append, value.dict)
    }

    public var description: String {
      let utf8Str = sourcekitd_request_description_copy(arr)!
      let result = String(cString: utf8Str)
      free(utf8Str)
      return result
    }

  }

  private let req = Dictionary()

  public init(uid: SourceKitdUID) {
    req.add(SourceKitdUID.key_Request, value: uid)
  }

  public func addParameter(_ key: SourceKitdUID, value: String) {
    req.add(key, value: value)
  }

  public func addParameter(_ key: SourceKitdUID, value: Int) {
    req.add(key, value: value)
  }

  public func addParameter(_ key: SourceKitdUID, value: SourceKitdUID) {
    req.add(key, value: value)
  }

  public func addArrayParameter(_ key: SourceKitdUID) -> Array {
    let arr = Array()
    req.add(key, value: arr)
    return arr
  }

  public func addDictionaryParameter(_ key: SourceKitdUID) -> Dictionary {
    let dict = Dictionary()
    req.add(key, value: dict)
    return dict
  }

  public var description: String {
    return req.description
  }

  public var rawRequest: sourcekitd_object_t {
    return req.dict
  }

  public func addCompilerArgsToRequest(_ compilerArguments: [String]?,
                                       _ bufferName: String? = nil) {
    let args = self.addArrayParameter(SourceKitdUID.key_CompilerArgs)

    if let compilerArguments = compilerArguments {
      for argument in compilerArguments {
        switch argument {
        // Exclude some arguments which SourceKit doesn't want or need.
        case "-Xfrontend":
          break
        default:
          args.add(argument)
        }
      }
    }
  }
}
