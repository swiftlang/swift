/**
 * Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
 * All Rights Reserved.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 **/

import WinSDK


//                                                +--------+
//                                            +-->+ vtable |
//                              +---------+   |   +--------+
//                          +-->+ lpvfTbl +---+
//          +-----------+   |   +---------+
// pThis+-->+ WinSDK.T  +---+
//          +-----------+
//          | Unmanaged +---+
//          +-----------+   |   +---------+
//                          +-->+ Swift.T +---+
//                              +---------+   |   +--------------+
//                                            +-->+ User Defined |
//                                                +--------------+

extension IFileOperationProgressSink {
  // Recover the unmanaged `IFileOperationProgressSink` Swift instance which the
  // COM instance is associated with.
  //
  // See Also: `IFileOperationProgressSink.Object`
  fileprivate static func from(_ pUnk: UnsafeMutableRawPointer?)
      -> Unmanaged<IFileOperationProgressSink>? {
    return pUnk?.assumingMemoryBound(to: IFileOperationProgressSink.Object.self)
              .pointee.wrapper
  }
}

// The COM vtable for the `IFileOperationProgressSink` that will be registered
// with COM when the Swift instance is created. The vtable simply converts the
// arguments to the wrapped Swift type and dispatches to the instance which is
// derived from the storage of the Swift object identified through the passed in
// instance pointer.
private var vtable: WinSDK.IFileOperationProgressSinkVtbl = .init(
  // MARK - IUnknown Methods

  QueryInterface: {
    guard let pUnk = $0, let riid = $1, let ppvObject = $2 else {
      return E_INVALIDARG
    }
    guard riid.pointee == IFileOperationProgressSink.IID else {
      ppvObject.pointee = nil
      return E_NOINTERFACE
    }
    _ = pUnk.pointee.lpVtbl.pointee.AddRef(pUnk)
    ppvObject.pointee = UnsafeMutableRawPointer(pUnk)
    return S_OK
  },
  AddRef: {
    var instance = IFileOperationProgressSink.from($0)!
    _ = instance.retain()
    return ULONG(_getRetainCount(instance.takeUnretainedValue()))
  },
  Release: {
    var instance = IFileOperationProgressSink.from($0)!
    return ULONG(_getRetainCount(instance.takeRetainedValue()))
  },

  // MARK - IFileOperationProgressSink Methods

  StartOperations: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.StartOperations()
  },
  FinishOperations: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.FinishOperations($1)
  },
  PreRenameItem: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.PreRenameItem($1, IShellItem(pUnk: $2),
                              String(decodingCString: $3!, as: UTF16.self))
  },
  PostRenameItem: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.PostRenameItem($1, IShellItem(pUnk: $2),
                               String(decodingCString: $3!, as: UTF16.self),
                               $4, IShellItem(pUnk: $5))
  },
  PreMoveItem: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.PreMoveItem($1, IShellItem(pUnk: $2), IShellItem(pUnk: $3),
                            String(decodingCString: $4!, as: UTF16.self))
  },
  PostMoveItem: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.PostMoveItem($1, IShellItem(pUnk: $2), IShellItem(pUnk: $3),
                             String(decodingCString: $4!, as: UTF16.self),
                             $5, IShellItem(pUnk: $6))
  },
  PreCopyItem: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.PreCopyItem($1, IShellItem(pUnk: $2), IShellItem(pUnk: $3),
                            String(decodingCString: $4!, as: UTF16.self))
  },
  PostCopyItem: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.PostCopyItem($1, IShellItem(pUnk: $2), IShellItem(pUnk: $3),
                             String(decodingCString: $4!, as: UTF16.self),
                             $5, IShellItem(pUnk: $6))
  },
  PreDeleteItem: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.PreDeleteItem($1, IShellItem(pUnk: $2))
  },
  PostDeleteItem: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.PostDeleteItem($1, IShellItem(pUnk: $2), $3,
                               IShellItem(pUnk: $4))
  },
  PreNewItem: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.PreNewItem($1, IShellItem(pUnk: $2),
                           String(decodingCString: $3!, as: UTF16.self))
  },
  PostNewItem: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.PostNewItem($1, IShellItem(pUnk: $2),
                            String(decodingCString: $3!, as: UTF16.self),
                            String(decodingCString: $4!, as: UTF16.self),
                            $5, $6, IShellItem(pUnk: $7))
  },
  UpdateProgress: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.UpdateProgress($1, $2)
  },
  ResetTimer: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.ResetTimer()
  },
  PauseTimer: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.PauseTimer()
  },
  ResumeTimer: {
    guard let self =
        IFileOperationProgressSink.from($0)?.takeUnretainedValue() else {
      return E_INVALIDARG
    }
    return self.ResumeTimer()
  }
)

/// Exposes methods that provide a rich notification system used by callers of
/// `IFileOperation` to monitor the details of the operations they are performing
/// through that interface.
open class IFileOperationProgressSink: IUnknown {
  override public class var IID: IID { IID_IFileOperationProgressSink }

  // Represents the COM object as passed to the COM system.
  //
  // The object consists of the WinSDK representation of the type itself and a
  // pointer to an unmanaged Swift object instance of the Swift class type
  // mirroring the COM object. The layout of this object is important as the
  // pointer to an instance of this type is passed to COM so that COM can treat
  // it as the WinSDK type while enabling Swift to recover the associated class
  // allocation.
  fileprivate struct Object {
    var `class`: WinSDK.IFileOperationProgressSink
    var wrapper: Unmanaged<IFileOperationProgressSink>?
  }
  fileprivate var instance: Object

  // MARK - Creating an `IFileOperationProgressSink`

  /// Creates an object conforming to the `IFileOperationProgressSink` COM
  /// interface.
  ///
  /// `pUnk` should be `nil` always, the instance being allocated provides the
  /// interface implementation.
  required public init(pUnk pointer: UnsafeMutableRawPointer? = nil) {
    self.instance = withUnsafeMutablePointer(to: &vtable) {
      Object(class: WinSDK.IFileOperationProgressSink(lpVtbl: $0))
    }
    super.init(pUnk: withUnsafeMutablePointer(to: &instance) { $0 })
    self.instance.wrapper =
        Unmanaged<IFileOperationProgressSink>.passUnretained(self)
  }

  /// Performs caller-implemented actions after the last operation perform by the
  /// call to `IFileOperation` is complete.
  open func FinishOperations(_ hrResult: HRESULT) -> HRESULT {
    return S_OK
  }

  /// Not supported.
  open func PauseTimer() -> HRESULT {
    return S_OK
  }

  /// Performs caller-implemented actions after the copy process for each item is
  /// complete.
  open func PostCopyItem(_ dwFlags: DWORD, _ psiItem: IShellItem,
                         _ psiDestinationFolder: IShellItem,
                         _ pszNewName: String, _ hrCopy: HRESULT,
                         _ psiNewlyCreated: IShellItem) -> HRESULT {
    return S_OK
  }

  /// Performs caller-implemented actions after the delete process for each item
  /// in complete.
  open func PostDeleteItem(_ dwFlags: DWORD, _ psiItem: IShellItem,
                           _ hrDelete: HRESULT, _ psiNewlyCreated: IShellItem)
      -> HRESULT {
    return S_OK
  }

  /// Performs caller-implemented actions after the move process for each item is
  /// complete.
  open func PostMoveItem(_ dwFlags: DWORD, _ psiItem: IShellItem,
                         _ psiDestinationFolder: IShellItem,
                         _ pszNewName: String, _ hrMove: HRESULT,
                         _ psiNewlyCreated: IShellItem) -> HRESULT {
    return S_OK
  }

  /// Performs caller-implemented actions after the new item is created.
  open func PostNewItem(_ dwFlags: DWORD, _ psiDestinationFolder: IShellItem,
                        _ pszNewName: String, _ pszTemplateName: String,
                        _ dwFileAttributes: DWORD, _ hrNew: HRESULT,
                        _ psiNewItem: IShellItem) -> HRESULT {
    return S_OK
  }

  /// Performs caller-implemented actions after the rename process for each item
  /// is complete.
  open func PostRenameItem(_ dwFlags: DWORD, _ psiItem: IShellItem,
                           _ pszNewName: String, _ hrRename: HRESULT,
                           _ psiNewlyCreated: IShellItem) -> HRESULT {
    return S_OK
  }

  /// Performs caller-implemented actions before the copy process for each item
  /// begins.
  open func PreCopyItem(_ dwFlags: DWORD, _ psiItem: IShellItem,
                        _ psiDestinationFolder: IShellItem,
                        _ pszNewName: String) -> HRESULT {
    return S_OK
  }

  /// Performs caller-implemented actions before the delete process for each item
  /// begins.
  open func PreDeleteItem(_ dwFlags: DWORD, _ psiItem: IShellItem) -> HRESULT {
    return S_OK
  }

  /// Performs caller-implemented actions before the move process for each item
  /// begins.
  open func PreMoveItem(_ dwFlags: DWORD, _ psiItem: IShellItem,
                        _ psiDestinationFolder: IShellItem,
                        _ pszNewName: String) -> HRESULT {
    return S_OK
  }

  /// Performs caller-implemented actions before the process to create a new item
  /// begins.
  open func PreNewItem(_ dwFlags: DWORD, _ psiDestinationFolder: IShellItem,
                       _ pszNewName: String) -> HRESULT {
    return S_OK
  }

  /// Performs caller-implemented actions before the rename process for each item
  /// begins.
  open func PreRenameItem(_ dwFlags: DWORD, _ psiItem: IShellItem,
                          _ pszNewName: String) -> HRESULT {
    return S_OK
  }

  /// Not supported.
  open func ResetTimer() -> HRESULT {
    return S_OK
  }

  /// Not supported.
  open func ResumeTimer() -> HRESULT {
    return S_OK
  }

  /// Performs caller-implemented actions before any specific file operations are
  /// performed.
  open func StartOperations() -> HRESULT {
    return S_OK
  }

  /// Provides an estimate of the total amount of work currently done in relation
  /// to the total amount of work.
  open func UpdateProgress(_ iWorkTotal: UINT, _ iWorkSoFar: UINT) -> HRESULT {
    return S_OK
  }
}
