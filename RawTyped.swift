/**
 * Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
 * All Rights Reserved.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 **/

import WinSDK

internal func RawPointer<T: IUnknown, U>(_ pUnk: T?)
    -> UnsafeMutablePointer<U>? {
  guard let pUnk = pUnk else { return nil }
  if let pUnk: UnsafeMutableRawPointer = UnsafeMutableRawPointer(pUnk.pUnk) {
    return pUnk.bindMemory(to: U.self, capacity: 1)
  }
  return nil
}
