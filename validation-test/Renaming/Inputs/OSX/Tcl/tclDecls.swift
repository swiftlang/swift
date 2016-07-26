
@discardableResult
func Tcl_PkgProvideEx(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ version: UnsafePointer<Int8>!, _ clientData: ClientData!) -> Int32
@discardableResult
func Tcl_PkgRequireEx(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ version: UnsafePointer<Int8>!, _ exact: Int32, _ clientDataPtr: UnsafeMutablePointer<ClientData?>!) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_Alloc(_ size: UInt32) -> UnsafeMutablePointer<Int8>!
func Tcl_Free(_ ptr: UnsafeMutablePointer<Int8>!)
@discardableResult
func Tcl_Realloc(_ ptr: UnsafeMutablePointer<Int8>!, _ size: UInt32) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_DbCkalloc(_ size: UInt32, _ file: UnsafePointer<Int8>!, _ line: Int32) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_DbCkfree(_ ptr: UnsafeMutablePointer<Int8>!, _ file: UnsafePointer<Int8>!, _ line: Int32) -> Int32
@discardableResult
func Tcl_DbCkrealloc(_ ptr: UnsafeMutablePointer<Int8>!, _ size: UInt32, _ file: UnsafePointer<Int8>!, _ line: Int32) -> UnsafeMutablePointer<Int8>!
func Tcl_CreateFileHandler(_ fd: Int32, _ mask: Int32, _ proc: (@convention(c) (ClientData!, Int32) -> Void)!, _ clientData: ClientData!)
func Tcl_DeleteFileHandler(_ fd: Int32)
func Tcl_SetTimer(_ timePtr: UnsafeMutablePointer<Tcl_Time>!)
func Tcl_Sleep(_ ms: Int32)
@discardableResult
func Tcl_WaitForEvent(_ timePtr: UnsafeMutablePointer<Tcl_Time>!) -> Int32
@discardableResult
func Tcl_AppendAllObjTypes(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
func Tcl_AppendToObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ bytes: UnsafePointer<Int8>!, _ length: Int32)
@discardableResult
func Tcl_ConcatObj(_ objc: Int32, _ objv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_ConvertToType(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ typePtr: UnsafeMutablePointer<Tcl_ObjType>!) -> Int32
func Tcl_DbDecrRefCount(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ file: UnsafePointer<Int8>!, _ line: Int32)
func Tcl_DbIncrRefCount(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ file: UnsafePointer<Int8>!, _ line: Int32)
@discardableResult
func Tcl_DbIsShared(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ file: UnsafePointer<Int8>!, _ line: Int32) -> Int32
@discardableResult
func Tcl_DbNewBooleanObj(_ boolValue: Int32, _ file: UnsafePointer<Int8>!, _ line: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_DbNewByteArrayObj(_ bytes: UnsafePointer<UInt8>!, _ length: Int32, _ file: UnsafePointer<Int8>!, _ line: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_DbNewDoubleObj(_ doubleValue: Double, _ file: UnsafePointer<Int8>!, _ line: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_DbNewListObj(_ objc: Int32, _ objv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, _ file: UnsafePointer<Int8>!, _ line: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_DbNewLongObj(_ longValue: Int, _ file: UnsafePointer<Int8>!, _ line: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_DbNewObj(_ file: UnsafePointer<Int8>!, _ line: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_DbNewStringObj(_ bytes: UnsafePointer<Int8>!, _ length: Int32, _ file: UnsafePointer<Int8>!, _ line: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_DuplicateObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!
func TclFreeObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!)
@discardableResult
func Tcl_GetBoolean(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ src: UnsafePointer<Int8>!, _ boolPtr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_GetBooleanFromObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ boolPtr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_GetByteArrayFromObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ lengthPtr: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<UInt8>!
@discardableResult
func Tcl_GetDouble(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ src: UnsafePointer<Int8>!, _ doublePtr: UnsafeMutablePointer<Double>!) -> Int32
@discardableResult
func Tcl_GetDoubleFromObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ doublePtr: UnsafeMutablePointer<Double>!) -> Int32
@discardableResult
func Tcl_GetIndexFromObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ tablePtr: UnsafeMutablePointer<UnsafePointer<Int8>?>!, _ msg: UnsafePointer<Int8>!, _ flags: Int32, _ indexPtr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_GetInt(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ src: UnsafePointer<Int8>!, _ intPtr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_GetIntFromObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ intPtr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_GetLongFromObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ longPtr: UnsafeMutablePointer<Int>!) -> Int32
@discardableResult
func Tcl_GetObjType(_ typeName: UnsafePointer<Int8>!) -> UnsafeMutablePointer<Tcl_ObjType>!
@discardableResult
func Tcl_GetStringFromObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ lengthPtr: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Int8>!
func Tcl_InvalidateStringRep(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!)
@discardableResult
func Tcl_ListObjAppendList(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ listPtr: UnsafeMutablePointer<Tcl_Obj>!, _ elemListPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_ListObjAppendElement(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ listPtr: UnsafeMutablePointer<Tcl_Obj>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_ListObjGetElements(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ listPtr: UnsafeMutablePointer<Tcl_Obj>!, _ objcPtr: UnsafeMutablePointer<Int32>!, _ objvPtr: UnsafeMutablePointer<UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>?>!) -> Int32
@discardableResult
func Tcl_ListObjIndex(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ listPtr: UnsafeMutablePointer<Tcl_Obj>!, _ index: Int32, _ objPtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32
@discardableResult
func Tcl_ListObjLength(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ listPtr: UnsafeMutablePointer<Tcl_Obj>!, _ lengthPtr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_ListObjReplace(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ listPtr: UnsafeMutablePointer<Tcl_Obj>!, _ first: Int32, _ count: Int32, _ objc: Int32, _ objv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32
@discardableResult
func Tcl_NewBooleanObj(_ boolValue: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_NewByteArrayObj(_ bytes: UnsafePointer<UInt8>!, _ length: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_NewDoubleObj(_ doubleValue: Double) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_NewIntObj(_ intValue: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_NewListObj(_ objc: Int32, _ objv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_NewLongObj(_ longValue: Int) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_NewObj() -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_NewStringObj(_ bytes: UnsafePointer<Int8>!, _ length: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
func Tcl_SetBooleanObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ boolValue: Int32)
@discardableResult
func Tcl_SetByteArrayLength(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ length: Int32) -> UnsafeMutablePointer<UInt8>!
func Tcl_SetByteArrayObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ bytes: UnsafePointer<UInt8>!, _ length: Int32)
func Tcl_SetDoubleObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ doubleValue: Double)
func Tcl_SetIntObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ intValue: Int32)
func Tcl_SetListObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ objc: Int32, _ objv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!)
func Tcl_SetLongObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ longValue: Int)
func Tcl_SetObjLength(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ length: Int32)
func Tcl_SetStringObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ bytes: UnsafePointer<Int8>!, _ length: Int32)
func Tcl_AddErrorInfo(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ message: UnsafePointer<Int8>!)
func Tcl_AddObjErrorInfo(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ message: UnsafePointer<Int8>!, _ length: Int32)
func Tcl_AllowExceptions(_ interp: UnsafeMutablePointer<Tcl_Interp>!)
func Tcl_AppendElement(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ element: UnsafePointer<Int8>!)
@discardableResult
func Tcl_AsyncCreate(_ proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!, _ clientData: ClientData!) -> Tcl_AsyncHandler!
func Tcl_AsyncDelete(_ async: Tcl_AsyncHandler!)
@discardableResult
func Tcl_AsyncInvoke(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ code: Int32) -> Int32
func Tcl_AsyncMark(_ async: Tcl_AsyncHandler!)
@discardableResult
func Tcl_AsyncReady() -> Int32
func Tcl_BackgroundError(_ interp: UnsafeMutablePointer<Tcl_Interp>!)
@discardableResult
func Tcl_Backslash(_ src: UnsafePointer<Int8>!, _ readPtr: UnsafeMutablePointer<Int32>!) -> Int8
@discardableResult
func Tcl_BadChannelOption(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ optionName: UnsafePointer<Int8>!, _ optionList: UnsafePointer<Int8>!) -> Int32
func Tcl_CallWhenDeleted(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, _ clientData: ClientData!)
func Tcl_CancelIdleCall(_ idleProc: (@convention(c) (ClientData!) -> Void)!, _ clientData: ClientData!)
@discardableResult
func Tcl_Close(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ chan: Tcl_Channel!) -> Int32
@discardableResult
func Tcl_CommandComplete(_ cmd: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_Concat(_ argc: Int32, _ argv: UnsafePointer<UnsafePointer<Int8>?>!) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_ConvertElement(_ src: UnsafePointer<Int8>!, _ dst: UnsafeMutablePointer<Int8>!, _ flags: Int32) -> Int32
@discardableResult
func Tcl_ConvertCountedElement(_ src: UnsafePointer<Int8>!, _ length: Int32, _ dst: UnsafeMutablePointer<Int8>!, _ flags: Int32) -> Int32
@discardableResult
func Tcl_CreateAlias(_ slave: UnsafeMutablePointer<Tcl_Interp>!, _ slaveCmd: UnsafePointer<Int8>!, _ target: UnsafeMutablePointer<Tcl_Interp>!, _ targetCmd: UnsafePointer<Int8>!, _ argc: Int32, _ argv: UnsafePointer<UnsafePointer<Int8>?>!) -> Int32
@discardableResult
func Tcl_CreateAliasObj(_ slave: UnsafeMutablePointer<Tcl_Interp>!, _ slaveCmd: UnsafePointer<Int8>!, _ target: UnsafeMutablePointer<Tcl_Interp>!, _ targetCmd: UnsafePointer<Int8>!, _ objc: Int32, _ objv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32
@discardableResult
func Tcl_CreateChannel(_ typePtr: UnsafeMutablePointer<Tcl_ChannelType>!, _ chanName: UnsafePointer<Int8>!, _ instanceData: ClientData!, _ mask: Int32) -> Tcl_Channel!
func Tcl_CreateChannelHandler(_ chan: Tcl_Channel!, _ mask: Int32, _ proc: (@convention(c) (ClientData!, Int32) -> Void)!, _ clientData: ClientData!)
func Tcl_CreateCloseHandler(_ chan: Tcl_Channel!, _ proc: (@convention(c) (ClientData!) -> Void)!, _ clientData: ClientData!)
@discardableResult
func Tcl_CreateCommand(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ cmdName: UnsafePointer<Int8>!, _ proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32)!, _ clientData: ClientData!, _ deleteProc: (@convention(c) (ClientData!) -> Void)!) -> Tcl_Command!
func Tcl_CreateEventSource(_ setupProc: (@convention(c) (ClientData!, Int32) -> Void)!, _ checkProc: (@convention(c) (ClientData!, Int32) -> Void)!, _ clientData: ClientData!)
func Tcl_CreateExitHandler(_ proc: (@convention(c) (ClientData!) -> Void)!, _ clientData: ClientData!)
@discardableResult
func Tcl_CreateInterp() -> UnsafeMutablePointer<Tcl_Interp>!
func Tcl_CreateMathFunc(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ numArgs: Int32, _ argTypes: UnsafeMutablePointer<Tcl_ValueType>!, _ proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Value>!, UnsafeMutablePointer<Tcl_Value>!) -> Int32)!, _ clientData: ClientData!)
@discardableResult
func Tcl_CreateObjCommand(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ cmdName: UnsafePointer<Int8>!, _ proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, _ clientData: ClientData!, _ deleteProc: (@convention(c) (ClientData!) -> Void)!) -> Tcl_Command!
@discardableResult
func Tcl_CreateSlave(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ slaveName: UnsafePointer<Int8>!, _ isSafe: Int32) -> UnsafeMutablePointer<Tcl_Interp>!
@discardableResult
func Tcl_CreateTimerHandler(_ milliseconds: Int32, _ proc: (@convention(c) (ClientData!) -> Void)!, _ clientData: ClientData!) -> Tcl_TimerToken!
@discardableResult
func Tcl_CreateTrace(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ level: Int32, _ proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<Int8>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32)!, ClientData!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Void)!, _ clientData: ClientData!) -> Tcl_Trace!
func Tcl_DeleteAssocData(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!)
func Tcl_DeleteChannelHandler(_ chan: Tcl_Channel!, _ proc: (@convention(c) (ClientData!, Int32) -> Void)!, _ clientData: ClientData!)
func Tcl_DeleteCloseHandler(_ chan: Tcl_Channel!, _ proc: (@convention(c) (ClientData!) -> Void)!, _ clientData: ClientData!)
@discardableResult
func Tcl_DeleteCommand(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ cmdName: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_DeleteCommandFromToken(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ command: Tcl_Command!) -> Int32
func Tcl_DeleteEvents(_ proc: (@convention(c) (UnsafeMutablePointer<Tcl_Event>!, ClientData!) -> Int32)!, _ clientData: ClientData!)
func Tcl_DeleteEventSource(_ setupProc: (@convention(c) (ClientData!, Int32) -> Void)!, _ checkProc: (@convention(c) (ClientData!, Int32) -> Void)!, _ clientData: ClientData!)
func Tcl_DeleteExitHandler(_ proc: (@convention(c) (ClientData!) -> Void)!, _ clientData: ClientData!)
func Tcl_DeleteHashEntry(_ entryPtr: UnsafeMutablePointer<Tcl_HashEntry>!)
func Tcl_DeleteHashTable(_ tablePtr: UnsafeMutablePointer<Tcl_HashTable>!)
func Tcl_DeleteInterp(_ interp: UnsafeMutablePointer<Tcl_Interp>!)
func Tcl_DetachPids(_ numPids: Int32, _ pidPtr: UnsafeMutablePointer<Tcl_Pid?>!)
func Tcl_DeleteTimerHandler(_ token: Tcl_TimerToken!)
func Tcl_DeleteTrace(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ trace: Tcl_Trace!)
func Tcl_DontCallWhenDeleted(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, _ clientData: ClientData!)
@discardableResult
func Tcl_DoOneEvent(_ flags: Int32) -> Int32
func Tcl_DoWhenIdle(_ proc: (@convention(c) (ClientData!) -> Void)!, _ clientData: ClientData!)
@discardableResult
func Tcl_DStringAppend(_ dsPtr: UnsafeMutablePointer<Tcl_DString>!, _ bytes: UnsafePointer<Int8>!, _ length: Int32) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_DStringAppendElement(_ dsPtr: UnsafeMutablePointer<Tcl_DString>!, _ element: UnsafePointer<Int8>!) -> UnsafeMutablePointer<Int8>!
func Tcl_DStringEndSublist(_ dsPtr: UnsafeMutablePointer<Tcl_DString>!)
func Tcl_DStringFree(_ dsPtr: UnsafeMutablePointer<Tcl_DString>!)
func Tcl_DStringGetResult(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ dsPtr: UnsafeMutablePointer<Tcl_DString>!)
func Tcl_DStringInit(_ dsPtr: UnsafeMutablePointer<Tcl_DString>!)
func Tcl_DStringResult(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ dsPtr: UnsafeMutablePointer<Tcl_DString>!)
func Tcl_DStringSetLength(_ dsPtr: UnsafeMutablePointer<Tcl_DString>!, _ length: Int32)
func Tcl_DStringStartSublist(_ dsPtr: UnsafeMutablePointer<Tcl_DString>!)
@discardableResult
func Tcl_Eof(_ chan: Tcl_Channel!) -> Int32
@discardableResult
func Tcl_ErrnoId() -> UnsafePointer<Int8>!
@discardableResult
func Tcl_ErrnoMsg(_ err: Int32) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_Eval(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ script: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_EvalFile(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ fileName: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_EvalObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
func Tcl_EventuallyFree(_ clientData: ClientData!, _ freeProc: (@convention(c) (UnsafeMutablePointer<Int8>!) -> Void)!)
func Tcl_Exit(_ status: Int32)
@discardableResult
func Tcl_ExposeCommand(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ hiddenCmdToken: UnsafePointer<Int8>!, _ cmdName: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_ExprBoolean(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ expr: UnsafePointer<Int8>!, _ ptr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_ExprBooleanObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ ptr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_ExprDouble(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ expr: UnsafePointer<Int8>!, _ ptr: UnsafeMutablePointer<Double>!) -> Int32
@discardableResult
func Tcl_ExprDoubleObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ ptr: UnsafeMutablePointer<Double>!) -> Int32
@discardableResult
func Tcl_ExprLong(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ expr: UnsafePointer<Int8>!, _ ptr: UnsafeMutablePointer<Int>!) -> Int32
@discardableResult
func Tcl_ExprLongObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ ptr: UnsafeMutablePointer<Int>!) -> Int32
@discardableResult
func Tcl_ExprObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ resultPtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32
@discardableResult
func Tcl_ExprString(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ expr: UnsafePointer<Int8>!) -> Int32
func Tcl_Finalize()
func Tcl_FindExecutable(_ argv0: UnsafePointer<Int8>!)
@discardableResult
func Tcl_FirstHashEntry(_ tablePtr: UnsafeMutablePointer<Tcl_HashTable>!, _ searchPtr: UnsafeMutablePointer<Tcl_HashSearch>!) -> UnsafeMutablePointer<Tcl_HashEntry>!
@discardableResult
func Tcl_Flush(_ chan: Tcl_Channel!) -> Int32
func Tcl_FreeResult(_ interp: UnsafeMutablePointer<Tcl_Interp>!)
@discardableResult
func Tcl_GetAlias(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ slaveCmd: UnsafePointer<Int8>!, _ targetInterpPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Interp>?>!, _ targetCmdPtr: UnsafeMutablePointer<UnsafePointer<Int8>?>!, _ argcPtr: UnsafeMutablePointer<Int32>!, _ argvPtr: UnsafeMutablePointer<UnsafeMutablePointer<UnsafePointer<Int8>?>?>!) -> Int32
@discardableResult
func Tcl_GetAliasObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ slaveCmd: UnsafePointer<Int8>!, _ targetInterpPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Interp>?>!, _ targetCmdPtr: UnsafeMutablePointer<UnsafePointer<Int8>?>!, _ objcPtr: UnsafeMutablePointer<Int32>!, _ objv: UnsafeMutablePointer<UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>?>!) -> Int32
@discardableResult
func Tcl_GetAssocData(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ procPtr: UnsafeMutablePointer<(@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)?>!) -> ClientData!
@discardableResult
func Tcl_GetChannel(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ chanName: UnsafePointer<Int8>!, _ modePtr: UnsafeMutablePointer<Int32>!) -> Tcl_Channel!
@discardableResult
func Tcl_GetChannelBufferSize(_ chan: Tcl_Channel!) -> Int32
@discardableResult
func Tcl_GetChannelHandle(_ chan: Tcl_Channel!, _ direction: Int32, _ handlePtr: UnsafeMutablePointer<ClientData?>!) -> Int32
@discardableResult
func Tcl_GetChannelInstanceData(_ chan: Tcl_Channel!) -> ClientData!
@discardableResult
func Tcl_GetChannelMode(_ chan: Tcl_Channel!) -> Int32
@discardableResult
func Tcl_GetChannelName(_ chan: Tcl_Channel!) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_GetChannelOption(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ chan: Tcl_Channel!, _ optionName: UnsafePointer<Int8>!, _ dsPtr: UnsafeMutablePointer<Tcl_DString>!) -> Int32
@discardableResult
func Tcl_GetChannelType(_ chan: Tcl_Channel!) -> UnsafeMutablePointer<Tcl_ChannelType>!
@discardableResult
func Tcl_GetCommandInfo(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ cmdName: UnsafePointer<Int8>!, _ infoPtr: UnsafeMutablePointer<Tcl_CmdInfo>!) -> Int32
@discardableResult
func Tcl_GetCommandName(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ command: Tcl_Command!) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_GetErrno() -> Int32
@discardableResult
func Tcl_GetHostName() -> UnsafePointer<Int8>!
@discardableResult
func Tcl_GetInterpPath(_ askInterp: UnsafeMutablePointer<Tcl_Interp>!, _ slaveInterp: UnsafeMutablePointer<Tcl_Interp>!) -> Int32
@discardableResult
func Tcl_GetMaster(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Interp>!
@discardableResult
func Tcl_GetNameOfExecutable() -> UnsafePointer<Int8>!
@discardableResult
func Tcl_GetObjResult(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_GetOpenFile(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ chanID: UnsafePointer<Int8>!, _ forWriting: Int32, _ checkUsage: Int32, _ filePtr: UnsafeMutablePointer<ClientData?>!) -> Int32
@discardableResult
func Tcl_GetPathType(_ path: UnsafePointer<Int8>!) -> Tcl_PathType
@discardableResult
func Tcl_Gets(_ chan: Tcl_Channel!, _ dsPtr: UnsafeMutablePointer<Tcl_DString>!) -> Int32
@discardableResult
func Tcl_GetsObj(_ chan: Tcl_Channel!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_GetServiceMode() -> Int32
@discardableResult
func Tcl_GetSlave(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ slaveName: UnsafePointer<Int8>!) -> UnsafeMutablePointer<Tcl_Interp>!
@discardableResult
func Tcl_GetStdChannel(_ type: Int32) -> Tcl_Channel!
@discardableResult
func Tcl_GetStringResult(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_GetVar(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ varName: UnsafePointer<Int8>!, _ flags: Int32) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_GetVar2(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ part1: UnsafePointer<Int8>!, _ part2: UnsafePointer<Int8>!, _ flags: Int32) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_GlobalEval(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ command: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_GlobalEvalObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_HideCommand(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ cmdName: UnsafePointer<Int8>!, _ hiddenCmdToken: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_Init(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> Int32
func Tcl_InitHashTable(_ tablePtr: UnsafeMutablePointer<Tcl_HashTable>!, _ keyType: Int32)
@discardableResult
func Tcl_InputBlocked(_ chan: Tcl_Channel!) -> Int32
@discardableResult
func Tcl_InputBuffered(_ chan: Tcl_Channel!) -> Int32
@discardableResult
func Tcl_InterpDeleted(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> Int32
@discardableResult
func Tcl_IsSafe(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> Int32
@discardableResult
func Tcl_JoinPath(_ argc: Int32, _ argv: UnsafePointer<UnsafePointer<Int8>?>!, _ resultPtr: UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_LinkVar(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ varName: UnsafePointer<Int8>!, _ addr: UnsafeMutablePointer<Int8>!, _ type: Int32) -> Int32
@discardableResult
func Tcl_MakeFileChannel(_ handle: ClientData!, _ mode: Int32) -> Tcl_Channel!
@discardableResult
func Tcl_MakeSafe(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> Int32
@discardableResult
func Tcl_MakeTcpClientChannel(_ tcpSocket: ClientData!) -> Tcl_Channel!
@discardableResult
func Tcl_Merge(_ argc: Int32, _ argv: UnsafePointer<UnsafePointer<Int8>?>!) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_NextHashEntry(_ searchPtr: UnsafeMutablePointer<Tcl_HashSearch>!) -> UnsafeMutablePointer<Tcl_HashEntry>!
func Tcl_NotifyChannel(_ channel: Tcl_Channel!, _ mask: Int32)
@discardableResult
func Tcl_ObjGetVar2(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ part1Ptr: UnsafeMutablePointer<Tcl_Obj>!, _ part2Ptr: UnsafeMutablePointer<Tcl_Obj>!, _ flags: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_ObjSetVar2(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ part1Ptr: UnsafeMutablePointer<Tcl_Obj>!, _ part2Ptr: UnsafeMutablePointer<Tcl_Obj>!, _ newValuePtr: UnsafeMutablePointer<Tcl_Obj>!, _ flags: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_OpenCommandChannel(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ argc: Int32, _ argv: UnsafeMutablePointer<UnsafePointer<Int8>?>!, _ flags: Int32) -> Tcl_Channel!
@discardableResult
func Tcl_OpenFileChannel(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ fileName: UnsafePointer<Int8>!, _ modeString: UnsafePointer<Int8>!, _ permissions: Int32) -> Tcl_Channel!
@discardableResult
func Tcl_OpenTcpClient(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ port: Int32, _ address: UnsafePointer<Int8>!, _ myaddr: UnsafePointer<Int8>!, _ myport: Int32, _ async: Int32) -> Tcl_Channel!
@discardableResult
func Tcl_OpenTcpServer(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ port: Int32, _ host: UnsafePointer<Int8>!, _ acceptProc: (@convention(c) (ClientData!, Tcl_Channel!, UnsafeMutablePointer<Int8>!, Int32) -> Void)!, _ callbackData: ClientData!) -> Tcl_Channel!
func Tcl_Preserve(_ data: ClientData!)
func Tcl_PrintDouble(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ value: Double, _ dst: UnsafeMutablePointer<Int8>!)
@discardableResult
func Tcl_PutEnv(_ assignment: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_PosixError(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> UnsafePointer<Int8>!
func Tcl_QueueEvent(_ evPtr: UnsafeMutablePointer<Tcl_Event>!, _ position: Tcl_QueuePosition)
@discardableResult
func Tcl_Read(_ chan: Tcl_Channel!, _ bufPtr: UnsafeMutablePointer<Int8>!, _ toRead: Int32) -> Int32
func Tcl_ReapDetachedProcs()
@discardableResult
func Tcl_RecordAndEval(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ cmd: UnsafePointer<Int8>!, _ flags: Int32) -> Int32
@discardableResult
func Tcl_RecordAndEvalObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ cmdPtr: UnsafeMutablePointer<Tcl_Obj>!, _ flags: Int32) -> Int32
func Tcl_RegisterChannel(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ chan: Tcl_Channel!)
func Tcl_RegisterObjType(_ typePtr: UnsafeMutablePointer<Tcl_ObjType>!)
@discardableResult
func Tcl_RegExpCompile(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ pattern: UnsafePointer<Int8>!) -> Tcl_RegExp!
@discardableResult
func Tcl_RegExpExec(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ regexp: Tcl_RegExp!, _ text: UnsafePointer<Int8>!, _ start: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_RegExpMatch(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ text: UnsafePointer<Int8>!, _ pattern: UnsafePointer<Int8>!) -> Int32
func Tcl_RegExpRange(_ regexp: Tcl_RegExp!, _ index: Int32, _ startPtr: UnsafeMutablePointer<UnsafePointer<Int8>?>!, _ endPtr: UnsafeMutablePointer<UnsafePointer<Int8>?>!)
func Tcl_Release(_ clientData: ClientData!)
func Tcl_ResetResult(_ interp: UnsafeMutablePointer<Tcl_Interp>!)
@discardableResult
func Tcl_ScanElement(_ str: UnsafePointer<Int8>!, _ flagPtr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_ScanCountedElement(_ str: UnsafePointer<Int8>!, _ length: Int32, _ flagPtr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_SeekOld(_ chan: Tcl_Channel!, _ offset: Int32, _ mode: Int32) -> Int32
@discardableResult
func Tcl_ServiceAll() -> Int32
@discardableResult
func Tcl_ServiceEvent(_ flags: Int32) -> Int32
func Tcl_SetAssocData(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, _ clientData: ClientData!)
func Tcl_SetChannelBufferSize(_ chan: Tcl_Channel!, _ sz: Int32)
@discardableResult
func Tcl_SetChannelOption(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ chan: Tcl_Channel!, _ optionName: UnsafePointer<Int8>!, _ newValue: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_SetCommandInfo(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ cmdName: UnsafePointer<Int8>!, _ infoPtr: UnsafePointer<Tcl_CmdInfo>!) -> Int32
func Tcl_SetErrno(_ err: Int32)
func Tcl_SetMaxBlockTime(_ timePtr: UnsafeMutablePointer<Tcl_Time>!)
func Tcl_SetPanicProc(_ panicProc: OpaquePointer!)
@discardableResult
func Tcl_SetRecursionLimit(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ depth: Int32) -> Int32
func Tcl_SetResult(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ result: UnsafeMutablePointer<Int8>!, _ freeProc: (@convention(c) (UnsafeMutablePointer<Int8>!) -> Void)!)
@discardableResult
func Tcl_SetServiceMode(_ mode: Int32) -> Int32
func Tcl_SetObjErrorCode(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ errorObjPtr: UnsafeMutablePointer<Tcl_Obj>!)
func Tcl_SetObjResult(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ resultObjPtr: UnsafeMutablePointer<Tcl_Obj>!)
func Tcl_SetStdChannel(_ channel: Tcl_Channel!, _ type: Int32)
@discardableResult
func Tcl_SetVar(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ varName: UnsafePointer<Int8>!, _ newValue: UnsafePointer<Int8>!, _ flags: Int32) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_SetVar2(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ part1: UnsafePointer<Int8>!, _ part2: UnsafePointer<Int8>!, _ newValue: UnsafePointer<Int8>!, _ flags: Int32) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_SignalId(_ sig: Int32) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_SignalMsg(_ sig: Int32) -> UnsafePointer<Int8>!
func Tcl_SourceRCFile(_ interp: UnsafeMutablePointer<Tcl_Interp>!)
@discardableResult
func Tcl_SplitList(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ listStr: UnsafePointer<Int8>!, _ argcPtr: UnsafeMutablePointer<Int32>!, _ argvPtr: UnsafeMutablePointer<UnsafeMutablePointer<UnsafePointer<Int8>?>?>!) -> Int32
func Tcl_SplitPath(_ path: UnsafePointer<Int8>!, _ argcPtr: UnsafeMutablePointer<Int32>!, _ argvPtr: UnsafeMutablePointer<UnsafeMutablePointer<UnsafePointer<Int8>?>?>!)
func Tcl_StaticPackage(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ pkgName: UnsafePointer<Int8>!, _ initProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!, _ safeInitProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!)
@discardableResult
func Tcl_StringMatch(_ str: UnsafePointer<Int8>!, _ pattern: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_TellOld(_ chan: Tcl_Channel!) -> Int32
@discardableResult
func Tcl_TraceVar(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ varName: UnsafePointer<Int8>!, _ flags: Int32, _ proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, _ clientData: ClientData!) -> Int32
@discardableResult
func Tcl_TraceVar2(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ part1: UnsafePointer<Int8>!, _ part2: UnsafePointer<Int8>!, _ flags: Int32, _ proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, _ clientData: ClientData!) -> Int32
@discardableResult
func Tcl_TranslateFileName(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ bufferPtr: UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_Ungets(_ chan: Tcl_Channel!, _ str: UnsafePointer<Int8>!, _ len: Int32, _ atHead: Int32) -> Int32
func Tcl_UnlinkVar(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ varName: UnsafePointer<Int8>!)
@discardableResult
func Tcl_UnregisterChannel(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ chan: Tcl_Channel!) -> Int32
@discardableResult
func Tcl_UnsetVar(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ varName: UnsafePointer<Int8>!, _ flags: Int32) -> Int32
@discardableResult
func Tcl_UnsetVar2(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ part1: UnsafePointer<Int8>!, _ part2: UnsafePointer<Int8>!, _ flags: Int32) -> Int32
func Tcl_UntraceVar(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ varName: UnsafePointer<Int8>!, _ flags: Int32, _ proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, _ clientData: ClientData!)
func Tcl_UntraceVar2(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ part1: UnsafePointer<Int8>!, _ part2: UnsafePointer<Int8>!, _ flags: Int32, _ proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, _ clientData: ClientData!)
func Tcl_UpdateLinkedVar(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ varName: UnsafePointer<Int8>!)
@discardableResult
func Tcl_UpVar(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ frameName: UnsafePointer<Int8>!, _ varName: UnsafePointer<Int8>!, _ localName: UnsafePointer<Int8>!, _ flags: Int32) -> Int32
@discardableResult
func Tcl_UpVar2(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ frameName: UnsafePointer<Int8>!, _ part1: UnsafePointer<Int8>!, _ part2: UnsafePointer<Int8>!, _ localName: UnsafePointer<Int8>!, _ flags: Int32) -> Int32
@discardableResult
func Tcl_VarTraceInfo(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ varName: UnsafePointer<Int8>!, _ flags: Int32, _ procPtr: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, _ prevClientData: ClientData!) -> ClientData!
@discardableResult
func Tcl_VarTraceInfo2(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ part1: UnsafePointer<Int8>!, _ part2: UnsafePointer<Int8>!, _ flags: Int32, _ procPtr: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, _ prevClientData: ClientData!) -> ClientData!
@discardableResult
func Tcl_Write(_ chan: Tcl_Channel!, _ s: UnsafePointer<Int8>!, _ slen: Int32) -> Int32
func Tcl_WrongNumArgs(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objc: Int32, _ objv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, _ message: UnsafePointer<Int8>!)
@discardableResult
func Tcl_DumpActiveMemory(_ fileName: UnsafePointer<Int8>!) -> Int32
func Tcl_ValidateAllMemory(_ file: UnsafePointer<Int8>!, _ line: Int32)
func Tcl_AppendResultVA(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ argList: CVaListPointer)
func Tcl_AppendStringsToObjVA(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ argList: CVaListPointer)
@discardableResult
func Tcl_HashStats(_ tablePtr: UnsafeMutablePointer<Tcl_HashTable>!) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_ParseVar(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ start: UnsafePointer<Int8>!, _ termPtr: UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_PkgPresent(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ version: UnsafePointer<Int8>!, _ exact: Int32) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_PkgPresentEx(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ version: UnsafePointer<Int8>!, _ exact: Int32, _ clientDataPtr: UnsafeMutablePointer<ClientData?>!) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_PkgProvide(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ version: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_PkgRequire(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ version: UnsafePointer<Int8>!, _ exact: Int32) -> UnsafePointer<Int8>!
func Tcl_SetErrorCodeVA(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ argList: CVaListPointer)
@discardableResult
func Tcl_VarEvalVA(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ argList: CVaListPointer) -> Int32
@discardableResult
func Tcl_WaitPid(_ pid: Tcl_Pid!, _ statPtr: UnsafeMutablePointer<Int32>!, _ options: Int32) -> Tcl_Pid!
func Tcl_PanicVA(_ format: UnsafePointer<Int8>!, _ argList: CVaListPointer)
func Tcl_GetVersion(_ major: UnsafeMutablePointer<Int32>!, _ minor: UnsafeMutablePointer<Int32>!, _ patchLevel: UnsafeMutablePointer<Int32>!, _ type: UnsafeMutablePointer<Int32>!)
func Tcl_InitMemory(_ interp: UnsafeMutablePointer<Tcl_Interp>!)
@discardableResult
func Tcl_StackChannel(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ typePtr: UnsafeMutablePointer<Tcl_ChannelType>!, _ instanceData: ClientData!, _ mask: Int32, _ prevChan: Tcl_Channel!) -> Tcl_Channel!
@discardableResult
func Tcl_UnstackChannel(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ chan: Tcl_Channel!) -> Int32
@discardableResult
func Tcl_GetStackedChannel(_ chan: Tcl_Channel!) -> Tcl_Channel!
func Tcl_SetMainLoop(_ proc: (@convention(c) () -> Void)!)
func Tcl_AppendObjToObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ appendObjPtr: UnsafeMutablePointer<Tcl_Obj>!)
@discardableResult
func Tcl_CreateEncoding(_ typePtr: UnsafePointer<Tcl_EncodingType>!) -> Tcl_Encoding!
func Tcl_CreateThreadExitHandler(_ proc: (@convention(c) (ClientData!) -> Void)!, _ clientData: ClientData!)
func Tcl_DeleteThreadExitHandler(_ proc: (@convention(c) (ClientData!) -> Void)!, _ clientData: ClientData!)
func Tcl_DiscardResult(_ statePtr: UnsafeMutablePointer<Tcl_SavedResult>!)
@discardableResult
func Tcl_EvalEx(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ script: UnsafePointer<Int8>!, _ numBytes: Int32, _ flags: Int32) -> Int32
@discardableResult
func Tcl_EvalObjv(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objc: Int32, _ objv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, _ flags: Int32) -> Int32
@discardableResult
func Tcl_EvalObjEx(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ flags: Int32) -> Int32
func Tcl_ExitThread(_ status: Int32)
@discardableResult
func Tcl_ExternalToUtf(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ encoding: Tcl_Encoding!, _ src: UnsafePointer<Int8>!, _ srcLen: Int32, _ flags: Int32, _ statePtr: UnsafeMutablePointer<Tcl_EncodingState?>!, _ dst: UnsafeMutablePointer<Int8>!, _ dstLen: Int32, _ srcReadPtr: UnsafeMutablePointer<Int32>!, _ dstWrotePtr: UnsafeMutablePointer<Int32>!, _ dstCharsPtr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_ExternalToUtfDString(_ encoding: Tcl_Encoding!, _ src: UnsafePointer<Int8>!, _ srcLen: Int32, _ dsPtr: UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!
func Tcl_FinalizeThread()
func Tcl_FinalizeNotifier(_ clientData: ClientData!)
func Tcl_FreeEncoding(_ encoding: Tcl_Encoding!)
@discardableResult
func Tcl_GetCurrentThread() -> Tcl_ThreadId!
@discardableResult
func Tcl_GetEncoding(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!) -> Tcl_Encoding!
@discardableResult
func Tcl_GetEncodingName(_ encoding: Tcl_Encoding!) -> UnsafePointer<Int8>!
func Tcl_GetEncodingNames(_ interp: UnsafeMutablePointer<Tcl_Interp>!)
@discardableResult
func Tcl_GetIndexFromObjStruct(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ tablePtr: UnsafePointer<Void>!, _ offset: Int32, _ msg: UnsafePointer<Int8>!, _ flags: Int32, _ indexPtr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_GetThreadData(_ keyPtr: UnsafeMutablePointer<Tcl_ThreadDataKey?>!, _ size: Int32) -> UnsafeMutablePointer<Void>!
@discardableResult
func Tcl_GetVar2Ex(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ part1: UnsafePointer<Int8>!, _ part2: UnsafePointer<Int8>!, _ flags: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_InitNotifier() -> ClientData!
func Tcl_MutexLock(_ mutexPtr: UnsafeMutablePointer<Tcl_Mutex?>!)
func Tcl_MutexUnlock(_ mutexPtr: UnsafeMutablePointer<Tcl_Mutex?>!)
func Tcl_ConditionNotify(_ condPtr: UnsafeMutablePointer<Tcl_Condition?>!)
func Tcl_ConditionWait(_ condPtr: UnsafeMutablePointer<Tcl_Condition?>!, _ mutexPtr: UnsafeMutablePointer<Tcl_Mutex?>!, _ timePtr: UnsafeMutablePointer<Tcl_Time>!)
@discardableResult
func Tcl_NumUtfChars(_ src: UnsafePointer<Int8>!, _ length: Int32) -> Int32
@discardableResult
func Tcl_ReadChars(_ channel: Tcl_Channel!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ charsToRead: Int32, _ appendFlag: Int32) -> Int32
func Tcl_RestoreResult(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ statePtr: UnsafeMutablePointer<Tcl_SavedResult>!)
func Tcl_SaveResult(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ statePtr: UnsafeMutablePointer<Tcl_SavedResult>!)
@discardableResult
func Tcl_SetSystemEncoding(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_SetVar2Ex(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ part1: UnsafePointer<Int8>!, _ part2: UnsafePointer<Int8>!, _ newValuePtr: UnsafeMutablePointer<Tcl_Obj>!, _ flags: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
func Tcl_ThreadAlert(_ threadId: Tcl_ThreadId!)
func Tcl_ThreadQueueEvent(_ threadId: Tcl_ThreadId!, _ evPtr: UnsafeMutablePointer<Tcl_Event>!, _ position: Tcl_QueuePosition)
@discardableResult
func Tcl_UniCharAtIndex(_ src: UnsafePointer<Int8>!, _ index: Int32) -> Tcl_UniChar
@discardableResult
func Tcl_UniCharToLower(_ ch: Int32) -> Tcl_UniChar
@discardableResult
func Tcl_UniCharToTitle(_ ch: Int32) -> Tcl_UniChar
@discardableResult
func Tcl_UniCharToUpper(_ ch: Int32) -> Tcl_UniChar
@discardableResult
func Tcl_UniCharToUtf(_ ch: Int32, _ buf: UnsafeMutablePointer<Int8>!) -> Int32
@discardableResult
func Tcl_UtfAtIndex(_ src: UnsafePointer<Int8>!, _ index: Int32) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_UtfCharComplete(_ src: UnsafePointer<Int8>!, _ length: Int32) -> Int32
@discardableResult
func Tcl_UtfBackslash(_ src: UnsafePointer<Int8>!, _ readPtr: UnsafeMutablePointer<Int32>!, _ dst: UnsafeMutablePointer<Int8>!) -> Int32
@discardableResult
func Tcl_UtfFindFirst(_ src: UnsafePointer<Int8>!, _ ch: Int32) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_UtfFindLast(_ src: UnsafePointer<Int8>!, _ ch: Int32) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_UtfNext(_ src: UnsafePointer<Int8>!) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_UtfPrev(_ src: UnsafePointer<Int8>!, _ start: UnsafePointer<Int8>!) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_UtfToExternal(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ encoding: Tcl_Encoding!, _ src: UnsafePointer<Int8>!, _ srcLen: Int32, _ flags: Int32, _ statePtr: UnsafeMutablePointer<Tcl_EncodingState?>!, _ dst: UnsafeMutablePointer<Int8>!, _ dstLen: Int32, _ srcReadPtr: UnsafeMutablePointer<Int32>!, _ dstWrotePtr: UnsafeMutablePointer<Int32>!, _ dstCharsPtr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_UtfToExternalDString(_ encoding: Tcl_Encoding!, _ src: UnsafePointer<Int8>!, _ srcLen: Int32, _ dsPtr: UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_UtfToLower(_ src: UnsafeMutablePointer<Int8>!) -> Int32
@discardableResult
func Tcl_UtfToTitle(_ src: UnsafeMutablePointer<Int8>!) -> Int32
@discardableResult
func Tcl_UtfToUniChar(_ src: UnsafePointer<Int8>!, _ chPtr: UnsafeMutablePointer<Tcl_UniChar>!) -> Int32
@discardableResult
func Tcl_UtfToUpper(_ src: UnsafeMutablePointer<Int8>!) -> Int32
@discardableResult
func Tcl_WriteChars(_ chan: Tcl_Channel!, _ src: UnsafePointer<Int8>!, _ srcLen: Int32) -> Int32
@discardableResult
func Tcl_WriteObj(_ chan: Tcl_Channel!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_GetString(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_GetDefaultEncodingDir() -> UnsafePointer<Int8>!
func Tcl_SetDefaultEncodingDir(_ path: UnsafePointer<Int8>!)
func Tcl_AlertNotifier(_ clientData: ClientData!)
func Tcl_ServiceModeHook(_ mode: Int32)
@discardableResult
func Tcl_UniCharIsAlnum(_ ch: Int32) -> Int32
@discardableResult
func Tcl_UniCharIsAlpha(_ ch: Int32) -> Int32
@discardableResult
func Tcl_UniCharIsDigit(_ ch: Int32) -> Int32
@discardableResult
func Tcl_UniCharIsLower(_ ch: Int32) -> Int32
@discardableResult
func Tcl_UniCharIsSpace(_ ch: Int32) -> Int32
@discardableResult
func Tcl_UniCharIsUpper(_ ch: Int32) -> Int32
@discardableResult
func Tcl_UniCharIsWordChar(_ ch: Int32) -> Int32
@discardableResult
func Tcl_UniCharLen(_ uniStr: UnsafePointer<Tcl_UniChar>!) -> Int32
@discardableResult
func Tcl_UniCharNcmp(_ ucs: UnsafePointer<Tcl_UniChar>!, _ uct: UnsafePointer<Tcl_UniChar>!, _ numChars: UInt) -> Int32
@discardableResult
func Tcl_UniCharToUtfDString(_ uniStr: UnsafePointer<Tcl_UniChar>!, _ uniLength: Int32, _ dsPtr: UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_UtfToUniCharDString(_ src: UnsafePointer<Int8>!, _ length: Int32, _ dsPtr: UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Tcl_UniChar>!
@discardableResult
func Tcl_GetRegExpFromObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ patObj: UnsafeMutablePointer<Tcl_Obj>!, _ flags: Int32) -> Tcl_RegExp!
@discardableResult
func Tcl_EvalTokens(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ tokenPtr: UnsafeMutablePointer<Tcl_Token>!, _ count: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
func Tcl_FreeParse(_ parsePtr: UnsafeMutablePointer<Tcl_Parse>!)
func Tcl_LogCommandInfo(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ script: UnsafePointer<Int8>!, _ command: UnsafePointer<Int8>!, _ length: Int32)
@discardableResult
func Tcl_ParseBraces(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ start: UnsafePointer<Int8>!, _ numBytes: Int32, _ parsePtr: UnsafeMutablePointer<Tcl_Parse>!, _ append: Int32, _ termPtr: UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32
@discardableResult
func Tcl_ParseCommand(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ start: UnsafePointer<Int8>!, _ numBytes: Int32, _ nested: Int32, _ parsePtr: UnsafeMutablePointer<Tcl_Parse>!) -> Int32
@discardableResult
func Tcl_ParseExpr(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ start: UnsafePointer<Int8>!, _ numBytes: Int32, _ parsePtr: UnsafeMutablePointer<Tcl_Parse>!) -> Int32
@discardableResult
func Tcl_ParseQuotedString(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ start: UnsafePointer<Int8>!, _ numBytes: Int32, _ parsePtr: UnsafeMutablePointer<Tcl_Parse>!, _ append: Int32, _ termPtr: UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32
@discardableResult
func Tcl_ParseVarName(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ start: UnsafePointer<Int8>!, _ numBytes: Int32, _ parsePtr: UnsafeMutablePointer<Tcl_Parse>!, _ append: Int32) -> Int32
@discardableResult
func Tcl_GetCwd(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ cwdPtr: UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_Chdir(_ dirName: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_Access(_ path: UnsafePointer<Int8>!, _ mode: Int32) -> Int32
@discardableResult
func Tcl_Stat(_ path: UnsafePointer<Int8>!, _ bufPtr: UnsafeMutablePointer<stat>!) -> Int32
@discardableResult
func Tcl_UtfNcmp(_ s1: UnsafePointer<Int8>!, _ s2: UnsafePointer<Int8>!, _ n: UInt) -> Int32
@discardableResult
func Tcl_UtfNcasecmp(_ s1: UnsafePointer<Int8>!, _ s2: UnsafePointer<Int8>!, _ n: UInt) -> Int32
@discardableResult
func Tcl_StringCaseMatch(_ str: UnsafePointer<Int8>!, _ pattern: UnsafePointer<Int8>!, _ nocase: Int32) -> Int32
@discardableResult
func Tcl_UniCharIsControl(_ ch: Int32) -> Int32
@discardableResult
func Tcl_UniCharIsGraph(_ ch: Int32) -> Int32
@discardableResult
func Tcl_UniCharIsPrint(_ ch: Int32) -> Int32
@discardableResult
func Tcl_UniCharIsPunct(_ ch: Int32) -> Int32
@discardableResult
func Tcl_RegExpExecObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ regexp: Tcl_RegExp!, _ textObj: UnsafeMutablePointer<Tcl_Obj>!, _ offset: Int32, _ nmatches: Int32, _ flags: Int32) -> Int32
func Tcl_RegExpGetInfo(_ regexp: Tcl_RegExp!, _ infoPtr: UnsafeMutablePointer<Tcl_RegExpInfo>!)
@discardableResult
func Tcl_NewUnicodeObj(_ unicode: UnsafePointer<Tcl_UniChar>!, _ numChars: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
func Tcl_SetUnicodeObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ unicode: UnsafePointer<Tcl_UniChar>!, _ numChars: Int32)
@discardableResult
func Tcl_GetCharLength(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_GetUniChar(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ index: Int32) -> Tcl_UniChar
@discardableResult
func Tcl_GetUnicode(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_UniChar>!
@discardableResult
func Tcl_GetRange(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ first: Int32, _ last: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
func Tcl_AppendUnicodeToObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ unicode: UnsafePointer<Tcl_UniChar>!, _ length: Int32)
@discardableResult
func Tcl_RegExpMatchObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ textObj: UnsafeMutablePointer<Tcl_Obj>!, _ patternObj: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
func Tcl_SetNotifier(_ notifierProcPtr: UnsafeMutablePointer<Tcl_NotifierProcs>!)
@discardableResult
func Tcl_GetAllocMutex() -> UnsafeMutablePointer<Tcl_Mutex?>!
@discardableResult
func Tcl_GetChannelNames(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> Int32
@discardableResult
func Tcl_GetChannelNamesEx(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ pattern: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_ProcObjCmd(_ clientData: ClientData!, _ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objc: Int32, _ objv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32
func Tcl_ConditionFinalize(_ condPtr: UnsafeMutablePointer<Tcl_Condition?>!)
func Tcl_MutexFinalize(_ mutex: UnsafeMutablePointer<Tcl_Mutex?>!)
@discardableResult
func Tcl_CreateThread(_ idPtr: UnsafeMutablePointer<Tcl_ThreadId?>!, _ proc: (@convention(c) (ClientData!) -> Void)!, _ clientData: ClientData!, _ stackSize: Int32, _ flags: Int32) -> Int32
@discardableResult
func Tcl_ReadRaw(_ chan: Tcl_Channel!, _ dst: UnsafeMutablePointer<Int8>!, _ bytesToRead: Int32) -> Int32
@discardableResult
func Tcl_WriteRaw(_ chan: Tcl_Channel!, _ src: UnsafePointer<Int8>!, _ srcLen: Int32) -> Int32
@discardableResult
func Tcl_GetTopChannel(_ chan: Tcl_Channel!) -> Tcl_Channel!
@discardableResult
func Tcl_ChannelBuffered(_ chan: Tcl_Channel!) -> Int32
@discardableResult
func Tcl_ChannelName(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_ChannelVersion(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> Tcl_ChannelTypeVersion!
@discardableResult
func Tcl_ChannelBlockModeProc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32) -> Int32)!
@discardableResult
func Tcl_ChannelCloseProc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!
@discardableResult
func Tcl_ChannelClose2Proc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!
@discardableResult
func Tcl_ChannelInputProc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!
@discardableResult
func Tcl_ChannelOutputProc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!
@discardableResult
func Tcl_ChannelSeekProc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!
@discardableResult
func Tcl_ChannelSetOptionProc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!
@discardableResult
func Tcl_ChannelGetOptionProc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_DString>!) -> Int32)!
@discardableResult
func Tcl_ChannelWatchProc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32) -> Void)!
@discardableResult
func Tcl_ChannelGetHandleProc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32, UnsafeMutablePointer<ClientData?>!) -> Int32)!
@discardableResult
func Tcl_ChannelFlushProc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!) -> Int32)!
@discardableResult
func Tcl_ChannelHandlerProc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32) -> Int32)!
@discardableResult
func Tcl_JoinThread(_ threadId: Tcl_ThreadId!, _ result: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_IsChannelShared(_ channel: Tcl_Channel!) -> Int32
@discardableResult
func Tcl_IsChannelRegistered(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ channel: Tcl_Channel!) -> Int32
func Tcl_CutChannel(_ channel: Tcl_Channel!)
func Tcl_SpliceChannel(_ channel: Tcl_Channel!)
func Tcl_ClearChannelHandlers(_ channel: Tcl_Channel!)
@discardableResult
func Tcl_IsChannelExisting(_ channelName: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_UniCharNcasecmp(_ ucs: UnsafePointer<Tcl_UniChar>!, _ uct: UnsafePointer<Tcl_UniChar>!, _ numChars: UInt) -> Int32
@discardableResult
func Tcl_UniCharCaseMatch(_ uniStr: UnsafePointer<Tcl_UniChar>!, _ uniPattern: UnsafePointer<Tcl_UniChar>!, _ nocase: Int32) -> Int32
@discardableResult
func Tcl_FindHashEntry(_ tablePtr: UnsafeMutablePointer<Tcl_HashTable>!, _ key: UnsafePointer<Int8>!) -> UnsafeMutablePointer<Tcl_HashEntry>!
@discardableResult
func Tcl_CreateHashEntry(_ tablePtr: UnsafeMutablePointer<Tcl_HashTable>!, _ key: UnsafePointer<Int8>!, _ newPtr: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Tcl_HashEntry>!
func Tcl_InitCustomHashTable(_ tablePtr: UnsafeMutablePointer<Tcl_HashTable>!, _ keyType: Int32, _ typePtr: UnsafeMutablePointer<Tcl_HashKeyType>!)
func Tcl_InitObjHashTable(_ tablePtr: UnsafeMutablePointer<Tcl_HashTable>!)
@discardableResult
func Tcl_CommandTraceInfo(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ varName: UnsafePointer<Int8>!, _ flags: Int32, _ procPtr: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Void)!, _ prevClientData: ClientData!) -> ClientData!
@discardableResult
func Tcl_TraceCommand(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ varName: UnsafePointer<Int8>!, _ flags: Int32, _ proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Void)!, _ clientData: ClientData!) -> Int32
func Tcl_UntraceCommand(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ varName: UnsafePointer<Int8>!, _ flags: Int32, _ proc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Void)!, _ clientData: ClientData!)
@discardableResult
func Tcl_AttemptAlloc(_ size: UInt32) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_AttemptDbCkalloc(_ size: UInt32, _ file: UnsafePointer<Int8>!, _ line: Int32) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_AttemptRealloc(_ ptr: UnsafeMutablePointer<Int8>!, _ size: UInt32) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_AttemptDbCkrealloc(_ ptr: UnsafeMutablePointer<Int8>!, _ size: UInt32, _ file: UnsafePointer<Int8>!, _ line: Int32) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Tcl_AttemptSetObjLength(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ length: Int32) -> Int32
@discardableResult
func Tcl_GetChannelThread(_ channel: Tcl_Channel!) -> Tcl_ThreadId!
@discardableResult
func Tcl_GetUnicodeFromObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ lengthPtr: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Tcl_UniChar>!
@discardableResult
func Tcl_GetMathFuncInfo(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ numArgsPtr: UnsafeMutablePointer<Int32>!, _ argTypesPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_ValueType>?>!, _ procPtr: UnsafeMutablePointer<(@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Value>!, UnsafeMutablePointer<Tcl_Value>!) -> Int32)?>!, _ clientDataPtr: UnsafeMutablePointer<ClientData?>!) -> Int32
@discardableResult
func Tcl_ListMathFuncs(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ pattern: UnsafePointer<Int8>!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_SubstObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ flags: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_DetachChannel(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ channel: Tcl_Channel!) -> Int32
@discardableResult
func Tcl_IsStandardChannel(_ channel: Tcl_Channel!) -> Int32
@discardableResult
func Tcl_FSCopyFile(_ srcPathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ destPathPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_FSCopyDirectory(_ srcPathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ destPathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ errorPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32
@discardableResult
func Tcl_FSCreateDirectory(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_FSDeleteFile(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_FSLoadFile(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ sym1: UnsafePointer<Int8>!, _ sym2: UnsafePointer<Int8>!, _ proc1Ptr: UnsafeMutablePointer<(@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)?>!, _ proc2Ptr: UnsafeMutablePointer<(@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)?>!, _ handlePtr: UnsafeMutablePointer<Tcl_LoadHandle?>!, _ unloadProcPtr: UnsafeMutablePointer<(@convention(c) (Tcl_LoadHandle!) -> Void)?>!) -> Int32
@discardableResult
func Tcl_FSMatchInDirectory(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ result: UnsafeMutablePointer<Tcl_Obj>!, _ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ pattern: UnsafePointer<Int8>!, _ types: UnsafeMutablePointer<Tcl_GlobTypeData>!) -> Int32
@discardableResult
func Tcl_FSLink(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ toPtr: UnsafeMutablePointer<Tcl_Obj>!, _ linkAction: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_FSRemoveDirectory(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ recursive: Int32, _ errorPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32
@discardableResult
func Tcl_FSRenameFile(_ srcPathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ destPathPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_FSLstat(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ buf: UnsafeMutablePointer<Tcl_StatBuf>!) -> Int32
@discardableResult
func Tcl_FSUtime(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ tval: UnsafeMutablePointer<utimbuf>!) -> Int32
@discardableResult
func Tcl_FSFileAttrsGet(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ index: Int32, _ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ objPtrRef: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32
@discardableResult
func Tcl_FSFileAttrsSet(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ index: Int32, _ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_FSFileAttrStrings(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ objPtrRef: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<UnsafePointer<Int8>?>!
@discardableResult
func Tcl_FSStat(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ buf: UnsafeMutablePointer<Tcl_StatBuf>!) -> Int32
@discardableResult
func Tcl_FSAccess(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ mode: Int32) -> Int32
@discardableResult
func Tcl_FSOpenFileChannel(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ modeString: UnsafePointer<Int8>!, _ permissions: Int32) -> Tcl_Channel!
@discardableResult
func Tcl_FSGetCwd(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_FSChdir(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_FSConvertToPathType(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ pathPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_FSJoinPath(_ listObj: UnsafeMutablePointer<Tcl_Obj>!, _ elements: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_FSSplitPath(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ lenPtr: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_FSEqualPaths(_ firstPtr: UnsafeMutablePointer<Tcl_Obj>!, _ secondPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_FSGetNormalizedPath(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ pathPtr: UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_FSJoinToPath(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ objc: Int32, _ objv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_FSGetInternalRep(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!, _ fsPtr: UnsafeMutablePointer<Tcl_Filesystem>!) -> ClientData!
@discardableResult
func Tcl_FSGetTranslatedPath(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ pathPtr: UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_FSEvalFile(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ fileName: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_FSNewNativePath(_ fromFilesystem: UnsafeMutablePointer<Tcl_Filesystem>!, _ clientData: ClientData!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_FSGetNativePath(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_FSFileSystemInfo(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_FSPathSeparator(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_FSListVolumes() -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_FSRegister(_ clientData: ClientData!, _ fsPtr: UnsafeMutablePointer<Tcl_Filesystem>!) -> Int32
@discardableResult
func Tcl_FSUnregister(_ fsPtr: UnsafeMutablePointer<Tcl_Filesystem>!) -> Int32
@discardableResult
func Tcl_FSData(_ fsPtr: UnsafeMutablePointer<Tcl_Filesystem>!) -> ClientData!
@discardableResult
func Tcl_FSGetTranslatedStringPath(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ pathPtr: UnsafeMutablePointer<Tcl_Obj>!) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_FSGetFileSystemForPath(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Filesystem>!
@discardableResult
func Tcl_FSGetPathType(_ pathPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Tcl_PathType
@discardableResult
func Tcl_OutputBuffered(_ chan: Tcl_Channel!) -> Int32
func Tcl_FSMountsChanged(_ fsPtr: UnsafeMutablePointer<Tcl_Filesystem>!)
@discardableResult
func Tcl_EvalTokensStandard(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ tokenPtr: UnsafeMutablePointer<Tcl_Token>!, _ count: Int32) -> Int32
func Tcl_GetTime(_ timeBuf: UnsafeMutablePointer<Tcl_Time>!)
@discardableResult
func Tcl_CreateObjTrace(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ level: Int32, _ flags: Int32, _ objProc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<Int8>!, Tcl_Command!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, _ clientData: ClientData!, _ delProc: (@convention(c) (ClientData!) -> Void)!) -> Tcl_Trace!
@discardableResult
func Tcl_GetCommandInfoFromToken(_ token: Tcl_Command!, _ infoPtr: UnsafeMutablePointer<Tcl_CmdInfo>!) -> Int32
@discardableResult
func Tcl_SetCommandInfoFromToken(_ token: Tcl_Command!, _ infoPtr: UnsafePointer<Tcl_CmdInfo>!) -> Int32
@discardableResult
func Tcl_DbNewWideIntObj(_ wideValue: Tcl_WideInt, _ file: UnsafePointer<Int8>!, _ line: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_GetWideIntFromObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ widePtr: UnsafeMutablePointer<Tcl_WideInt>!) -> Int32
@discardableResult
func Tcl_NewWideIntObj(_ wideValue: Tcl_WideInt) -> UnsafeMutablePointer<Tcl_Obj>!
func Tcl_SetWideIntObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ wideValue: Tcl_WideInt)
@discardableResult
func Tcl_AllocStatBuf() -> UnsafeMutablePointer<Tcl_StatBuf>!
@discardableResult
func Tcl_Seek(_ chan: Tcl_Channel!, _ offset: Tcl_WideInt, _ mode: Int32) -> Tcl_WideInt
@discardableResult
func Tcl_Tell(_ chan: Tcl_Channel!) -> Tcl_WideInt
@discardableResult
func Tcl_ChannelWideSeekProc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Tcl_WideInt, Int32, UnsafeMutablePointer<Int32>!) -> Tcl_WideInt)!
@discardableResult
func Tcl_DictObjPut(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ dictPtr: UnsafeMutablePointer<Tcl_Obj>!, _ keyPtr: UnsafeMutablePointer<Tcl_Obj>!, _ valuePtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_DictObjGet(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ dictPtr: UnsafeMutablePointer<Tcl_Obj>!, _ keyPtr: UnsafeMutablePointer<Tcl_Obj>!, _ valuePtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32
@discardableResult
func Tcl_DictObjRemove(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ dictPtr: UnsafeMutablePointer<Tcl_Obj>!, _ keyPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_DictObjSize(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ dictPtr: UnsafeMutablePointer<Tcl_Obj>!, _ sizePtr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_DictObjFirst(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ dictPtr: UnsafeMutablePointer<Tcl_Obj>!, _ searchPtr: UnsafeMutablePointer<Tcl_DictSearch>!, _ keyPtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!, _ valuePtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!, _ donePtr: UnsafeMutablePointer<Int32>!) -> Int32
func Tcl_DictObjNext(_ searchPtr: UnsafeMutablePointer<Tcl_DictSearch>!, _ keyPtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!, _ valuePtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!, _ donePtr: UnsafeMutablePointer<Int32>!)
func Tcl_DictObjDone(_ searchPtr: UnsafeMutablePointer<Tcl_DictSearch>!)
@discardableResult
func Tcl_DictObjPutKeyList(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ dictPtr: UnsafeMutablePointer<Tcl_Obj>!, _ keyc: Int32, _ keyv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, _ valuePtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_DictObjRemoveKeyList(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ dictPtr: UnsafeMutablePointer<Tcl_Obj>!, _ keyc: Int32, _ keyv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32
@discardableResult
func Tcl_NewDictObj() -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_DbNewDictObj(_ file: UnsafePointer<Int8>!, _ line: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
func Tcl_RegisterConfig(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ pkgName: UnsafePointer<Int8>!, _ configuration: UnsafeMutablePointer<Tcl_Config>!, _ valEncoding: UnsafePointer<Int8>!)
@discardableResult
func Tcl_CreateNamespace(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ clientData: ClientData!, _ deleteProc: (@convention(c) (ClientData!) -> Void)!) -> UnsafeMutablePointer<Tcl_Namespace>!
func Tcl_DeleteNamespace(_ nsPtr: UnsafeMutablePointer<Tcl_Namespace>!)
@discardableResult
func Tcl_AppendExportList(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ nsPtr: UnsafeMutablePointer<Tcl_Namespace>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_Export(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ nsPtr: UnsafeMutablePointer<Tcl_Namespace>!, _ pattern: UnsafePointer<Int8>!, _ resetListFirst: Int32) -> Int32
@discardableResult
func Tcl_Import(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ nsPtr: UnsafeMutablePointer<Tcl_Namespace>!, _ pattern: UnsafePointer<Int8>!, _ allowOverwrite: Int32) -> Int32
@discardableResult
func Tcl_ForgetImport(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ nsPtr: UnsafeMutablePointer<Tcl_Namespace>!, _ pattern: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_GetCurrentNamespace(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Namespace>!
@discardableResult
func Tcl_GetGlobalNamespace(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Namespace>!
@discardableResult
func Tcl_FindNamespace(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ contextNsPtr: UnsafeMutablePointer<Tcl_Namespace>!, _ flags: Int32) -> UnsafeMutablePointer<Tcl_Namespace>!
@discardableResult
func Tcl_FindCommand(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ contextNsPtr: UnsafeMutablePointer<Tcl_Namespace>!, _ flags: Int32) -> Tcl_Command!
@discardableResult
func Tcl_GetCommandFromObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Tcl_Command!
func Tcl_GetCommandFullName(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ command: Tcl_Command!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!)
@discardableResult
func Tcl_FSEvalFileEx(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ fileName: UnsafeMutablePointer<Tcl_Obj>!, _ encodingName: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Tcl_SetExitProc(_ proc: (@convention(c) (ClientData!) -> Void)!) -> (@convention(c) (ClientData!) -> Void)!
func Tcl_LimitAddHandler(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ type: Int32, _ handlerProc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, _ clientData: ClientData!, _ deleteProc: (@convention(c) (ClientData!) -> Void)!)
func Tcl_LimitRemoveHandler(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ type: Int32, _ handlerProc: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, _ clientData: ClientData!)
@discardableResult
func Tcl_LimitReady(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> Int32
@discardableResult
func Tcl_LimitCheck(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> Int32
@discardableResult
func Tcl_LimitExceeded(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> Int32
func Tcl_LimitSetCommands(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ commandLimit: Int32)
func Tcl_LimitSetTime(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ timeLimitPtr: UnsafeMutablePointer<Tcl_Time>!)
func Tcl_LimitSetGranularity(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ type: Int32, _ granularity: Int32)
@discardableResult
func Tcl_LimitTypeEnabled(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ type: Int32) -> Int32
@discardableResult
func Tcl_LimitTypeExceeded(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ type: Int32) -> Int32
func Tcl_LimitTypeSet(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ type: Int32)
func Tcl_LimitTypeReset(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ type: Int32)
@discardableResult
func Tcl_LimitGetCommands(_ interp: UnsafeMutablePointer<Tcl_Interp>!) -> Int32
func Tcl_LimitGetTime(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ timeLimitPtr: UnsafeMutablePointer<Tcl_Time>!)
@discardableResult
func Tcl_LimitGetGranularity(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ type: Int32) -> Int32
@discardableResult
func Tcl_SaveInterpState(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ status: Int32) -> Tcl_InterpState!
@discardableResult
func Tcl_RestoreInterpState(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ state: Tcl_InterpState!) -> Int32
func Tcl_DiscardInterpState(_ state: Tcl_InterpState!)
@discardableResult
func Tcl_SetReturnOptions(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ options: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_GetReturnOptions(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ result: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_IsEnsemble(_ token: Tcl_Command!) -> Int32
@discardableResult
func Tcl_CreateEnsemble(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ namespacePtr: UnsafeMutablePointer<Tcl_Namespace>!, _ flags: Int32) -> Tcl_Command!
@discardableResult
func Tcl_FindEnsemble(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ cmdNameObj: UnsafeMutablePointer<Tcl_Obj>!, _ flags: Int32) -> Tcl_Command!
@discardableResult
func Tcl_SetEnsembleSubcommandList(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ token: Tcl_Command!, _ subcmdList: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_SetEnsembleMappingDict(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ token: Tcl_Command!, _ mapDict: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_SetEnsembleUnknownHandler(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ token: Tcl_Command!, _ unknownList: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_SetEnsembleFlags(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ token: Tcl_Command!, _ flags: Int32) -> Int32
@discardableResult
func Tcl_GetEnsembleSubcommandList(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ token: Tcl_Command!, _ subcmdListPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32
@discardableResult
func Tcl_GetEnsembleMappingDict(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ token: Tcl_Command!, _ mapDictPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32
@discardableResult
func Tcl_GetEnsembleUnknownHandler(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ token: Tcl_Command!, _ unknownListPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32
@discardableResult
func Tcl_GetEnsembleFlags(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ token: Tcl_Command!, _ flagsPtr: UnsafeMutablePointer<Int32>!) -> Int32
@discardableResult
func Tcl_GetEnsembleNamespace(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ token: Tcl_Command!, _ namespacePtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Namespace>?>!) -> Int32
func Tcl_SetTimeProc(_ getProc: (@convention(c) (UnsafeMutablePointer<Tcl_Time>!, ClientData!) -> Void)!, _ scaleProc: (@convention(c) (UnsafeMutablePointer<Tcl_Time>!, ClientData!) -> Void)!, _ clientData: ClientData!)
func Tcl_QueryTimeProc(_ getProc: UnsafeMutablePointer<(@convention(c) (UnsafeMutablePointer<Tcl_Time>!, ClientData!) -> Void)?>!, _ scaleProc: UnsafeMutablePointer<(@convention(c) (UnsafeMutablePointer<Tcl_Time>!, ClientData!) -> Void)?>!, _ clientData: UnsafeMutablePointer<ClientData?>!)
@discardableResult
func Tcl_ChannelThreadActionProc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32) -> Void)!
@discardableResult
func Tcl_NewBignumObj(_ value: UnsafeMutablePointer<mp_int>!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_DbNewBignumObj(_ value: UnsafeMutablePointer<mp_int>!, _ file: UnsafePointer<Int8>!, _ line: Int32) -> UnsafeMutablePointer<Tcl_Obj>!
func Tcl_SetBignumObj(_ obj: UnsafeMutablePointer<Tcl_Obj>!, _ value: UnsafeMutablePointer<mp_int>!)
@discardableResult
func Tcl_GetBignumFromObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ obj: UnsafeMutablePointer<Tcl_Obj>!, _ value: UnsafeMutablePointer<mp_int>!) -> Int32
@discardableResult
func Tcl_TakeBignumFromObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ obj: UnsafeMutablePointer<Tcl_Obj>!, _ value: UnsafeMutablePointer<mp_int>!) -> Int32
@discardableResult
func Tcl_TruncateChannel(_ chan: Tcl_Channel!, _ length: Tcl_WideInt) -> Int32
@discardableResult
func Tcl_ChannelTruncateProc(_ chanTypePtr: UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Tcl_WideInt) -> Int32)!
func Tcl_SetChannelErrorInterp(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ msg: UnsafeMutablePointer<Tcl_Obj>!)
func Tcl_GetChannelErrorInterp(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ msg: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!)
func Tcl_SetChannelError(_ chan: Tcl_Channel!, _ msg: UnsafeMutablePointer<Tcl_Obj>!)
func Tcl_GetChannelError(_ chan: Tcl_Channel!, _ msg: UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!)
@discardableResult
func Tcl_InitBignumFromDouble(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ initval: Double, _ toInit: UnsafeMutablePointer<mp_int>!) -> Int32
@discardableResult
func Tcl_GetNamespaceUnknownHandler(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ nsPtr: UnsafeMutablePointer<Tcl_Namespace>!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_SetNamespaceUnknownHandler(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ nsPtr: UnsafeMutablePointer<Tcl_Namespace>!, _ handlerPtr: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_GetEncodingFromObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ encodingPtr: UnsafeMutablePointer<Tcl_Encoding?>!) -> Int32
@discardableResult
func Tcl_GetEncodingSearchPath() -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_SetEncodingSearchPath(_ searchPath: UnsafeMutablePointer<Tcl_Obj>!) -> Int32
@discardableResult
func Tcl_GetEncodingNameFromEnvironment(_ bufPtr: UnsafeMutablePointer<Tcl_DString>!) -> UnsafePointer<Int8>!
@discardableResult
func Tcl_PkgRequireProc(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ name: UnsafePointer<Int8>!, _ objc: Int32, _ objv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, _ clientDataPtr: UnsafeMutablePointer<ClientData?>!) -> Int32
func Tcl_AppendObjToErrorInfo(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!)
func Tcl_AppendLimitedToObj(_ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ bytes: UnsafePointer<Int8>!, _ length: Int32, _ limit: Int32, _ ellipsis: UnsafePointer<Int8>!)
@discardableResult
func Tcl_Format(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ format: UnsafePointer<Int8>!, _ objc: Int32, _ objv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<Tcl_Obj>!
@discardableResult
func Tcl_AppendFormatToObj(_ interp: UnsafeMutablePointer<Tcl_Interp>!, _ objPtr: UnsafeMutablePointer<Tcl_Obj>!, _ format: UnsafePointer<Int8>!, _ objc: Int32, _ objv: UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32
struct TclStubHooks {
  var tclPlatStubs: UnsafeMutablePointer<TclPlatStubs>!
  var tclIntStubs: OpaquePointer!
  var tclIntPlatStubs: OpaquePointer!
  init()
}
struct TclStubs {
  var magic: Int32
  var hooks: UnsafeMutablePointer<TclStubHooks>!
  var tcl_PkgProvideEx: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, ClientData!) -> Int32)!
  var tcl_PkgRequireEx: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<ClientData?>!) -> UnsafePointer<Int8>!)!
  var tcl_Panic: OpaquePointer!
  var tcl_Alloc: (@convention(c) (UInt32) -> UnsafeMutablePointer<Int8>!)!
  var tcl_Free: (@convention(c) (UnsafeMutablePointer<Int8>!) -> Void)!
  var tcl_Realloc: (@convention(c) (UnsafeMutablePointer<Int8>!, UInt32) -> UnsafeMutablePointer<Int8>!)!
  var tcl_DbCkalloc: (@convention(c) (UInt32, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!
  var tcl_DbCkfree: (@convention(c) (UnsafeMutablePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_DbCkrealloc: (@convention(c) (UnsafeMutablePointer<Int8>!, UInt32, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!
  var tcl_CreateFileHandler: (@convention(c) (Int32, Int32, (@convention(c) (ClientData!, Int32) -> Void)!, ClientData!) -> Void)!
  var tcl_DeleteFileHandler: (@convention(c) (Int32) -> Void)!
  var tcl_SetTimer: (@convention(c) (UnsafeMutablePointer<Tcl_Time>!) -> Void)!
  var tcl_Sleep: (@convention(c) (Int32) -> Void)!
  var tcl_WaitForEvent: (@convention(c) (UnsafeMutablePointer<Tcl_Time>!) -> Int32)!
  var tcl_AppendAllObjTypes: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_AppendStringsToObj: OpaquePointer!
  var tcl_AppendToObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32) -> Void)!
  var tcl_ConcatObj: (@convention(c) (Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_ConvertToType: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_ObjType>!) -> Int32)!
  var tcl_DbDecrRefCount: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32) -> Void)!
  var tcl_DbIncrRefCount: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32) -> Void)!
  var tcl_DbIsShared: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_DbNewBooleanObj: (@convention(c) (Int32, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_DbNewByteArrayObj: (@convention(c) (UnsafePointer<UInt8>!, Int32, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_DbNewDoubleObj: (@convention(c) (Double, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_DbNewListObj: (@convention(c) (Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_DbNewLongObj: (@convention(c) (Int, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_DbNewObj: (@convention(c) (UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_DbNewStringObj: (@convention(c) (UnsafePointer<Int8>!, Int32, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_DuplicateObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tclFreeObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Void)!
  var tcl_GetBoolean: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_GetBooleanFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_GetByteArrayFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<UInt8>!)!
  var tcl_GetDouble: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Double>!) -> Int32)!
  var tcl_GetDoubleFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Double>!) -> Int32)!
  var tcl_GetIndexFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafePointer<Int8>?>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_GetInt: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_GetIntFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_GetLongFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int>!) -> Int32)!
  var tcl_GetObjType: (@convention(c) (UnsafePointer<Int8>!) -> UnsafeMutablePointer<Tcl_ObjType>!)!
  var tcl_GetStringFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Int8>!)!
  var tcl_InvalidateStringRep: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Void)!
  var tcl_ListObjAppendList: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_ListObjAppendElement: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_ListObjGetElements: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>?>!) -> Int32)!
  var tcl_ListObjIndex: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var tcl_ListObjLength: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_ListObjReplace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32, Int32, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var tcl_NewBooleanObj: (@convention(c) (Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_NewByteArrayObj: (@convention(c) (UnsafePointer<UInt8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_NewDoubleObj: (@convention(c) (Double) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_NewIntObj: (@convention(c) (Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_NewListObj: (@convention(c) (Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_NewLongObj: (@convention(c) (Int) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_NewObj: (@convention(c) () -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_NewStringObj: (@convention(c) (UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_SetBooleanObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Void)!
  var tcl_SetByteArrayLength: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<UInt8>!)!
  var tcl_SetByteArrayObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<UInt8>!, Int32) -> Void)!
  var tcl_SetDoubleObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Double) -> Void)!
  var tcl_SetIntObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Void)!
  var tcl_SetListObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Void)!
  var tcl_SetLongObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int) -> Void)!
  var tcl_SetObjLength: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Void)!
  var tcl_SetStringObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32) -> Void)!
  var tcl_AddErrorInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Void)!
  var tcl_AddObjErrorInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32) -> Void)!
  var tcl_AllowExceptions: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!
  var tcl_AppendElement: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Void)!
  var tcl_AppendResult: OpaquePointer!
  var tcl_AsyncCreate: (@convention(c) ((@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!, ClientData!) -> Tcl_AsyncHandler!)!
  var tcl_AsyncDelete: (@convention(c) (Tcl_AsyncHandler!) -> Void)!
  var tcl_AsyncInvoke: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!
  var tcl_AsyncMark: (@convention(c) (Tcl_AsyncHandler!) -> Void)!
  var tcl_AsyncReady: (@convention(c) () -> Int32)!
  var tcl_BackgroundError: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!
  var tcl_Backslash: (@convention(c) (UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> Int8)!
  var tcl_BadChannelOption: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_CallWhenDeleted: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, ClientData!) -> Void)!
  var tcl_CancelIdleCall: (@convention(c) ((@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!
  var tcl_Close: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!) -> Int32)!
  var tcl_CommandComplete: (@convention(c) (UnsafePointer<Int8>!) -> Int32)!
  var tcl_Concat: (@convention(c) (Int32, UnsafePointer<UnsafePointer<Int8>?>!) -> UnsafeMutablePointer<Int8>!)!
  var tcl_ConvertElement: (@convention(c) (UnsafePointer<Int8>!, UnsafeMutablePointer<Int8>!, Int32) -> Int32)!
  var tcl_ConvertCountedElement: (@convention(c) (UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Int8>!, Int32) -> Int32)!
  var tcl_CreateAlias: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafePointer<UnsafePointer<Int8>?>!) -> Int32)!
  var tcl_CreateAliasObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var tcl_CreateChannel: (@convention(c) (UnsafeMutablePointer<Tcl_ChannelType>!, UnsafePointer<Int8>!, ClientData!, Int32) -> Tcl_Channel!)!
  var tcl_CreateChannelHandler: (@convention(c) (Tcl_Channel!, Int32, (@convention(c) (ClientData!, Int32) -> Void)!, ClientData!) -> Void)!
  var tcl_CreateCloseHandler: (@convention(c) (Tcl_Channel!, (@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!
  var tcl_CreateCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32)!, ClientData!, (@convention(c) (ClientData!) -> Void)!) -> Tcl_Command!)!
  var tcl_CreateEventSource: (@convention(c) ((@convention(c) (ClientData!, Int32) -> Void)!, (@convention(c) (ClientData!, Int32) -> Void)!, ClientData!) -> Void)!
  var tcl_CreateExitHandler: (@convention(c) ((@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!
  var tcl_CreateInterp: (@convention(c) () -> UnsafeMutablePointer<Tcl_Interp>!)!
  var tcl_CreateMathFunc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_ValueType>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Value>!, UnsafeMutablePointer<Tcl_Value>!) -> Int32)!, ClientData!) -> Void)!
  var tcl_CreateObjCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, ClientData!, (@convention(c) (ClientData!) -> Void)!) -> Tcl_Command!)!
  var tcl_CreateSlave: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Interp>!)!
  var tcl_CreateTimerHandler: (@convention(c) (Int32, (@convention(c) (ClientData!) -> Void)!, ClientData!) -> Tcl_TimerToken!)!
  var tcl_CreateTrace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<Int8>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32)!, ClientData!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Void)!, ClientData!) -> Tcl_Trace!)!
  var tcl_DeleteAssocData: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Void)!
  var tcl_DeleteChannelHandler: (@convention(c) (Tcl_Channel!, (@convention(c) (ClientData!, Int32) -> Void)!, ClientData!) -> Void)!
  var tcl_DeleteCloseHandler: (@convention(c) (Tcl_Channel!, (@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!
  var tcl_DeleteCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_DeleteCommandFromToken: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!) -> Int32)!
  var tcl_DeleteEvents: (@convention(c) ((@convention(c) (UnsafeMutablePointer<Tcl_Event>!, ClientData!) -> Int32)!, ClientData!) -> Void)!
  var tcl_DeleteEventSource: (@convention(c) ((@convention(c) (ClientData!, Int32) -> Void)!, (@convention(c) (ClientData!, Int32) -> Void)!, ClientData!) -> Void)!
  var tcl_DeleteExitHandler: (@convention(c) ((@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!
  var tcl_DeleteHashEntry: (@convention(c) (UnsafeMutablePointer<Tcl_HashEntry>!) -> Void)!
  var tcl_DeleteHashTable: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!) -> Void)!
  var tcl_DeleteInterp: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!
  var tcl_DetachPids: (@convention(c) (Int32, UnsafeMutablePointer<Tcl_Pid?>!) -> Void)!
  var tcl_DeleteTimerHandler: (@convention(c) (Tcl_TimerToken!) -> Void)!
  var tcl_DeleteTrace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Trace!) -> Void)!
  var tcl_DontCallWhenDeleted: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, ClientData!) -> Void)!
  var tcl_DoOneEvent: (@convention(c) (Int32) -> Int32)!
  var tcl_DoWhenIdle: (@convention(c) ((@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!
  var tcl_DStringAppend: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!
  var tcl_DStringAppendElement: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!, UnsafePointer<Int8>!) -> UnsafeMutablePointer<Int8>!)!
  var tcl_DStringEndSublist: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!) -> Void)!
  var tcl_DStringFree: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!) -> Void)!
  var tcl_DStringGetResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_DString>!) -> Void)!
  var tcl_DStringInit: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!) -> Void)!
  var tcl_DStringResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_DString>!) -> Void)!
  var tcl_DStringSetLength: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!, Int32) -> Void)!
  var tcl_DStringStartSublist: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!) -> Void)!
  var tcl_Eof: (@convention(c) (Tcl_Channel!) -> Int32)!
  var tcl_ErrnoId: (@convention(c) () -> UnsafePointer<Int8>!)!
  var tcl_ErrnoMsg: (@convention(c) (Int32) -> UnsafePointer<Int8>!)!
  var tcl_Eval: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_EvalFile: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_EvalObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_EventuallyFree: (@convention(c) (ClientData!, (@convention(c) (UnsafeMutablePointer<Int8>!) -> Void)!) -> Void)!
  var tcl_Exit: (@convention(c) (Int32) -> Void)!
  var tcl_ExposeCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_ExprBoolean: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_ExprBooleanObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_ExprDouble: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Double>!) -> Int32)!
  var tcl_ExprDoubleObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Double>!) -> Int32)!
  var tcl_ExprLong: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int>!) -> Int32)!
  var tcl_ExprLongObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int>!) -> Int32)!
  var tcl_ExprObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var tcl_ExprString: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_Finalize: (@convention(c) () -> Void)!
  var tcl_FindExecutable: (@convention(c) (UnsafePointer<Int8>!) -> Void)!
  var tcl_FirstHashEntry: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, UnsafeMutablePointer<Tcl_HashSearch>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)!
  var tcl_Flush: (@convention(c) (Tcl_Channel!) -> Int32)!
  var tcl_FreeResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!
  var tcl_GetAlias: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Interp>?>!, UnsafeMutablePointer<UnsafePointer<Int8>?>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<UnsafeMutablePointer<UnsafePointer<Int8>?>?>!) -> Int32)!
  var tcl_GetAliasObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Interp>?>!, UnsafeMutablePointer<UnsafePointer<Int8>?>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>?>!) -> Int32)!
  var tcl_GetAssocData: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<(@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)?>!) -> ClientData!)!
  var tcl_GetChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> Tcl_Channel!)!
  var tcl_GetChannelBufferSize: (@convention(c) (Tcl_Channel!) -> Int32)!
  var tcl_GetChannelHandle: (@convention(c) (Tcl_Channel!, Int32, UnsafeMutablePointer<ClientData?>!) -> Int32)!
  var tcl_GetChannelInstanceData: (@convention(c) (Tcl_Channel!) -> ClientData!)!
  var tcl_GetChannelMode: (@convention(c) (Tcl_Channel!) -> Int32)!
  var tcl_GetChannelName: (@convention(c) (Tcl_Channel!) -> UnsafePointer<Int8>!)!
  var tcl_GetChannelOption: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_DString>!) -> Int32)!
  var tcl_GetChannelType: (@convention(c) (Tcl_Channel!) -> UnsafeMutablePointer<Tcl_ChannelType>!)!
  var tcl_GetCommandInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_CmdInfo>!) -> Int32)!
  var tcl_GetCommandName: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!) -> UnsafePointer<Int8>!)!
  var tcl_GetErrno: (@convention(c) () -> Int32)!
  var tcl_GetHostName: (@convention(c) () -> UnsafePointer<Int8>!)!
  var tcl_GetInterpPath: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!
  var tcl_GetMaster: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Interp>!)!
  var tcl_GetNameOfExecutable: (@convention(c) () -> UnsafePointer<Int8>!)!
  var tcl_GetObjResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_GetOpenFile: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, Int32, UnsafeMutablePointer<ClientData?>!) -> Int32)!
  var tcl_GetPathType: (@convention(c) (UnsafePointer<Int8>!) -> Tcl_PathType)!
  var tcl_Gets: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<Tcl_DString>!) -> Int32)!
  var tcl_GetsObj: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_GetServiceMode: (@convention(c) () -> Int32)!
  var tcl_GetSlave: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> UnsafeMutablePointer<Tcl_Interp>!)!
  var tcl_GetStdChannel: (@convention(c) (Int32) -> Tcl_Channel!)!
  var tcl_GetStringResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafePointer<Int8>!)!
  var tcl_GetVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!
  var tcl_GetVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!
  var tcl_GlobalEval: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_GlobalEvalObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_HideCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_Init: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!
  var tcl_InitHashTable: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, Int32) -> Void)!
  var tcl_InputBlocked: (@convention(c) (Tcl_Channel!) -> Int32)!
  var tcl_InputBuffered: (@convention(c) (Tcl_Channel!) -> Int32)!
  var tcl_InterpDeleted: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!
  var tcl_IsSafe: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!
  var tcl_JoinPath: (@convention(c) (Int32, UnsafePointer<UnsafePointer<Int8>?>!, UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!)!
  var tcl_LinkVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int8>!, Int32) -> Int32)!
  var reserved188: UnsafeMutablePointer<Void>!
  var tcl_MakeFileChannel: (@convention(c) (ClientData!, Int32) -> Tcl_Channel!)!
  var tcl_MakeSafe: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!
  var tcl_MakeTcpClientChannel: (@convention(c) (ClientData!) -> Tcl_Channel!)!
  var tcl_Merge: (@convention(c) (Int32, UnsafePointer<UnsafePointer<Int8>?>!) -> UnsafeMutablePointer<Int8>!)!
  var tcl_NextHashEntry: (@convention(c) (UnsafeMutablePointer<Tcl_HashSearch>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)!
  var tcl_NotifyChannel: (@convention(c) (Tcl_Channel!, Int32) -> Void)!
  var tcl_ObjGetVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_ObjSetVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_OpenCommandChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!, Int32) -> Tcl_Channel!)!
  var tcl_OpenFileChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Tcl_Channel!)!
  var tcl_OpenTcpClient: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32, Int32) -> Tcl_Channel!)!
  var tcl_OpenTcpServer: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<Int8>!, (@convention(c) (ClientData!, Tcl_Channel!, UnsafeMutablePointer<Int8>!, Int32) -> Void)!, ClientData!) -> Tcl_Channel!)!
  var tcl_Preserve: (@convention(c) (ClientData!) -> Void)!
  var tcl_PrintDouble: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Double, UnsafeMutablePointer<Int8>!) -> Void)!
  var tcl_PutEnv: (@convention(c) (UnsafePointer<Int8>!) -> Int32)!
  var tcl_PosixError: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafePointer<Int8>!)!
  var tcl_QueueEvent: (@convention(c) (UnsafeMutablePointer<Tcl_Event>!, Tcl_QueuePosition) -> Void)!
  var tcl_Read: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<Int8>!, Int32) -> Int32)!
  var tcl_ReapDetachedProcs: (@convention(c) () -> Void)!
  var tcl_RecordAndEval: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_RecordAndEvalObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Int32)!
  var tcl_RegisterChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!) -> Void)!
  var tcl_RegisterObjType: (@convention(c) (UnsafeMutablePointer<Tcl_ObjType>!) -> Void)!
  var tcl_RegExpCompile: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Tcl_RegExp!)!
  var tcl_RegExpExec: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_RegExp!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_RegExpMatch: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_RegExpRange: (@convention(c) (Tcl_RegExp!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Void)!
  var tcl_Release: (@convention(c) (ClientData!) -> Void)!
  var tcl_ResetResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!
  var tcl_ScanElement: (@convention(c) (UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_ScanCountedElement: (@convention(c) (UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_SeekOld: (@convention(c) (Tcl_Channel!, Int32, Int32) -> Int32)!
  var tcl_ServiceAll: (@convention(c) () -> Int32)!
  var tcl_ServiceEvent: (@convention(c) (Int32) -> Int32)!
  var tcl_SetAssocData: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, ClientData!) -> Void)!
  var tcl_SetChannelBufferSize: (@convention(c) (Tcl_Channel!, Int32) -> Void)!
  var tcl_SetChannelOption: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_SetCommandInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Tcl_CmdInfo>!) -> Int32)!
  var tcl_SetErrno: (@convention(c) (Int32) -> Void)!
  var tcl_SetErrorCode: OpaquePointer!
  var tcl_SetMaxBlockTime: (@convention(c) (UnsafeMutablePointer<Tcl_Time>!) -> Void)!
  var tcl_SetPanicProc: (@convention(c) (OpaquePointer!) -> Void)!
  var tcl_SetRecursionLimit: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!
  var tcl_SetResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Int8>!, (@convention(c) (UnsafeMutablePointer<Int8>!) -> Void)!) -> Void)!
  var tcl_SetServiceMode: (@convention(c) (Int32) -> Int32)!
  var tcl_SetObjErrorCode: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!
  var tcl_SetObjResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!
  var tcl_SetStdChannel: (@convention(c) (Tcl_Channel!, Int32) -> Void)!
  var tcl_SetVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!
  var tcl_SetVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!
  var tcl_SignalId: (@convention(c) (Int32) -> UnsafePointer<Int8>!)!
  var tcl_SignalMsg: (@convention(c) (Int32) -> UnsafePointer<Int8>!)!
  var tcl_SourceRCFile: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!
  var tcl_SplitList: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<UnsafeMutablePointer<UnsafePointer<Int8>?>?>!) -> Int32)!
  var tcl_SplitPath: (@convention(c) (UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<UnsafeMutablePointer<UnsafePointer<Int8>?>?>!) -> Void)!
  var tcl_StaticPackage: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!, (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!) -> Void)!
  var tcl_StringMatch: (@convention(c) (UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_TellOld: (@convention(c) (Tcl_Channel!) -> Int32)!
  var tcl_TraceVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, ClientData!) -> Int32)!
  var tcl_TraceVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, ClientData!) -> Int32)!
  var tcl_TranslateFileName: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!)!
  var tcl_Ungets: (@convention(c) (Tcl_Channel!, UnsafePointer<Int8>!, Int32, Int32) -> Int32)!
  var tcl_UnlinkVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Void)!
  var tcl_UnregisterChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!) -> Int32)!
  var tcl_UnsetVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_UnsetVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_UntraceVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, ClientData!) -> Void)!
  var tcl_UntraceVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, ClientData!) -> Void)!
  var tcl_UpdateLinkedVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Void)!
  var tcl_UpVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_UpVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_VarEval: OpaquePointer!
  var tcl_VarTraceInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, ClientData!) -> ClientData!)!
  var tcl_VarTraceInfo2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, ClientData!) -> ClientData!)!
  var tcl_Write: (@convention(c) (Tcl_Channel!, UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_WrongNumArgs: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafePointer<Int8>!) -> Void)!
  var tcl_DumpActiveMemory: (@convention(c) (UnsafePointer<Int8>!) -> Int32)!
  var tcl_ValidateAllMemory: (@convention(c) (UnsafePointer<Int8>!, Int32) -> Void)!
  var tcl_AppendResultVA: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, CVaListPointer) -> Void)!
  var tcl_AppendStringsToObjVA: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, CVaListPointer) -> Void)!
  var tcl_HashStats: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!) -> UnsafeMutablePointer<Int8>!)!
  var tcl_ParseVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> UnsafePointer<Int8>!)!
  var tcl_PkgPresent: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!
  var tcl_PkgPresentEx: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<ClientData?>!) -> UnsafePointer<Int8>!)!
  var tcl_PkgProvide: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_PkgRequire: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!
  var tcl_SetErrorCodeVA: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, CVaListPointer) -> Void)!
  var tcl_VarEvalVA: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, CVaListPointer) -> Int32)!
  var tcl_WaitPid: (@convention(c) (Tcl_Pid!, UnsafeMutablePointer<Int32>!, Int32) -> Tcl_Pid!)!
  var tcl_PanicVA: (@convention(c) (UnsafePointer<Int8>!, CVaListPointer) -> Void)!
  var tcl_GetVersion: (@convention(c) (UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!) -> Void)!
  var tcl_InitMemory: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!
  var tcl_StackChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_ChannelType>!, ClientData!, Int32, Tcl_Channel!) -> Tcl_Channel!)!
  var tcl_UnstackChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!) -> Int32)!
  var tcl_GetStackedChannel: (@convention(c) (Tcl_Channel!) -> Tcl_Channel!)!
  var tcl_SetMainLoop: (@convention(c) ((@convention(c) () -> Void)!) -> Void)!
  var reserved285: UnsafeMutablePointer<Void>!
  var tcl_AppendObjToObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!
  var tcl_CreateEncoding: (@convention(c) (UnsafePointer<Tcl_EncodingType>!) -> Tcl_Encoding!)!
  var tcl_CreateThreadExitHandler: (@convention(c) ((@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!
  var tcl_DeleteThreadExitHandler: (@convention(c) ((@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!
  var tcl_DiscardResult: (@convention(c) (UnsafeMutablePointer<Tcl_SavedResult>!) -> Void)!
  var tcl_EvalEx: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, Int32) -> Int32)!
  var tcl_EvalObjv: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, Int32) -> Int32)!
  var tcl_EvalObjEx: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Int32)!
  var tcl_ExitThread: (@convention(c) (Int32) -> Void)!
  var tcl_ExternalToUtf: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Encoding!, UnsafePointer<Int8>!, Int32, Int32, UnsafeMutablePointer<Tcl_EncodingState?>!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_ExternalToUtfDString: (@convention(c) (Tcl_Encoding!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!)!
  var tcl_FinalizeThread: (@convention(c) () -> Void)!
  var tcl_FinalizeNotifier: (@convention(c) (ClientData!) -> Void)!
  var tcl_FreeEncoding: (@convention(c) (Tcl_Encoding!) -> Void)!
  var tcl_GetCurrentThread: (@convention(c) () -> Tcl_ThreadId!)!
  var tcl_GetEncoding: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Tcl_Encoding!)!
  var tcl_GetEncodingName: (@convention(c) (Tcl_Encoding!) -> UnsafePointer<Int8>!)!
  var tcl_GetEncodingNames: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!
  var tcl_GetIndexFromObjStruct: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Void>!, Int32, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_GetThreadData: (@convention(c) (UnsafeMutablePointer<Tcl_ThreadDataKey?>!, Int32) -> UnsafeMutablePointer<Void>!)!
  var tcl_GetVar2Ex: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_InitNotifier: (@convention(c) () -> ClientData!)!
  var tcl_MutexLock: (@convention(c) (UnsafeMutablePointer<Tcl_Mutex?>!) -> Void)!
  var tcl_MutexUnlock: (@convention(c) (UnsafeMutablePointer<Tcl_Mutex?>!) -> Void)!
  var tcl_ConditionNotify: (@convention(c) (UnsafeMutablePointer<Tcl_Condition?>!) -> Void)!
  var tcl_ConditionWait: (@convention(c) (UnsafeMutablePointer<Tcl_Condition?>!, UnsafeMutablePointer<Tcl_Mutex?>!, UnsafeMutablePointer<Tcl_Time>!) -> Void)!
  var tcl_NumUtfChars: (@convention(c) (UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_ReadChars: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<Tcl_Obj>!, Int32, Int32) -> Int32)!
  var tcl_RestoreResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_SavedResult>!) -> Void)!
  var tcl_SaveResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_SavedResult>!) -> Void)!
  var tcl_SetSystemEncoding: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_SetVar2Ex: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_ThreadAlert: (@convention(c) (Tcl_ThreadId!) -> Void)!
  var tcl_ThreadQueueEvent: (@convention(c) (Tcl_ThreadId!, UnsafeMutablePointer<Tcl_Event>!, Tcl_QueuePosition) -> Void)!
  var tcl_UniCharAtIndex: (@convention(c) (UnsafePointer<Int8>!, Int32) -> Tcl_UniChar)!
  var tcl_UniCharToLower: (@convention(c) (Int32) -> Tcl_UniChar)!
  var tcl_UniCharToTitle: (@convention(c) (Int32) -> Tcl_UniChar)!
  var tcl_UniCharToUpper: (@convention(c) (Int32) -> Tcl_UniChar)!
  var tcl_UniCharToUtf: (@convention(c) (Int32, UnsafeMutablePointer<Int8>!) -> Int32)!
  var tcl_UtfAtIndex: (@convention(c) (UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!
  var tcl_UtfCharComplete: (@convention(c) (UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_UtfBackslash: (@convention(c) (UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int8>!) -> Int32)!
  var tcl_UtfFindFirst: (@convention(c) (UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!
  var tcl_UtfFindLast: (@convention(c) (UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!
  var tcl_UtfNext: (@convention(c) (UnsafePointer<Int8>!) -> UnsafePointer<Int8>!)!
  var tcl_UtfPrev: (@convention(c) (UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> UnsafePointer<Int8>!)!
  var tcl_UtfToExternal: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Encoding!, UnsafePointer<Int8>!, Int32, Int32, UnsafeMutablePointer<Tcl_EncodingState?>!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_UtfToExternalDString: (@convention(c) (Tcl_Encoding!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!)!
  var tcl_UtfToLower: (@convention(c) (UnsafeMutablePointer<Int8>!) -> Int32)!
  var tcl_UtfToTitle: (@convention(c) (UnsafeMutablePointer<Int8>!) -> Int32)!
  var tcl_UtfToUniChar: (@convention(c) (UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_UniChar>!) -> Int32)!
  var tcl_UtfToUpper: (@convention(c) (UnsafeMutablePointer<Int8>!) -> Int32)!
  var tcl_WriteChars: (@convention(c) (Tcl_Channel!, UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_WriteObj: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_GetString: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Int8>!)!
  var tcl_GetDefaultEncodingDir: (@convention(c) () -> UnsafePointer<Int8>!)!
  var tcl_SetDefaultEncodingDir: (@convention(c) (UnsafePointer<Int8>!) -> Void)!
  var tcl_AlertNotifier: (@convention(c) (ClientData!) -> Void)!
  var tcl_ServiceModeHook: (@convention(c) (Int32) -> Void)!
  var tcl_UniCharIsAlnum: (@convention(c) (Int32) -> Int32)!
  var tcl_UniCharIsAlpha: (@convention(c) (Int32) -> Int32)!
  var tcl_UniCharIsDigit: (@convention(c) (Int32) -> Int32)!
  var tcl_UniCharIsLower: (@convention(c) (Int32) -> Int32)!
  var tcl_UniCharIsSpace: (@convention(c) (Int32) -> Int32)!
  var tcl_UniCharIsUpper: (@convention(c) (Int32) -> Int32)!
  var tcl_UniCharIsWordChar: (@convention(c) (Int32) -> Int32)!
  var tcl_UniCharLen: (@convention(c) (UnsafePointer<Tcl_UniChar>!) -> Int32)!
  var tcl_UniCharNcmp: (@convention(c) (UnsafePointer<Tcl_UniChar>!, UnsafePointer<Tcl_UniChar>!, UInt) -> Int32)!
  var tcl_UniCharToUtfDString: (@convention(c) (UnsafePointer<Tcl_UniChar>!, Int32, UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!)!
  var tcl_UtfToUniCharDString: (@convention(c) (UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Tcl_UniChar>!)!
  var tcl_GetRegExpFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Tcl_RegExp!)!
  var tcl_EvalTokens: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Token>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_FreeParse: (@convention(c) (UnsafeMutablePointer<Tcl_Parse>!) -> Void)!
  var tcl_LogCommandInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Void)!
  var tcl_ParseBraces: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_Parse>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32)!
  var tcl_ParseCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, Int32, UnsafeMutablePointer<Tcl_Parse>!) -> Int32)!
  var tcl_ParseExpr: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_Parse>!) -> Int32)!
  var tcl_ParseQuotedString: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_Parse>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32)!
  var tcl_ParseVarName: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_Parse>!, Int32) -> Int32)!
  var tcl_GetCwd: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!)!
  var tcl_Chdir: (@convention(c) (UnsafePointer<Int8>!) -> Int32)!
  var tcl_Access: (@convention(c) (UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_Stat: (@convention(c) (UnsafePointer<Int8>!, UnsafeMutablePointer<stat>!) -> Int32)!
  var tcl_UtfNcmp: (@convention(c) (UnsafePointer<Int8>!, UnsafePointer<Int8>!, UInt) -> Int32)!
  var tcl_UtfNcasecmp: (@convention(c) (UnsafePointer<Int8>!, UnsafePointer<Int8>!, UInt) -> Int32)!
  var tcl_StringCaseMatch: (@convention(c) (UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_UniCharIsControl: (@convention(c) (Int32) -> Int32)!
  var tcl_UniCharIsGraph: (@convention(c) (Int32) -> Int32)!
  var tcl_UniCharIsPrint: (@convention(c) (Int32) -> Int32)!
  var tcl_UniCharIsPunct: (@convention(c) (Int32) -> Int32)!
  var tcl_RegExpExecObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_RegExp!, UnsafeMutablePointer<Tcl_Obj>!, Int32, Int32, Int32) -> Int32)!
  var tcl_RegExpGetInfo: (@convention(c) (Tcl_RegExp!, UnsafeMutablePointer<Tcl_RegExpInfo>!) -> Void)!
  var tcl_NewUnicodeObj: (@convention(c) (UnsafePointer<Tcl_UniChar>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_SetUnicodeObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Tcl_UniChar>!, Int32) -> Void)!
  var tcl_GetCharLength: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_GetUniChar: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Tcl_UniChar)!
  var tcl_GetUnicode: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_UniChar>!)!
  var tcl_GetRange: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_AppendUnicodeToObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Tcl_UniChar>!, Int32) -> Void)!
  var tcl_RegExpMatchObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_SetNotifier: (@convention(c) (UnsafeMutablePointer<Tcl_NotifierProcs>!) -> Void)!
  var tcl_GetAllocMutex: (@convention(c) () -> UnsafeMutablePointer<Tcl_Mutex?>!)!
  var tcl_GetChannelNames: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!
  var tcl_GetChannelNamesEx: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_ProcObjCmd: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var tcl_ConditionFinalize: (@convention(c) (UnsafeMutablePointer<Tcl_Condition?>!) -> Void)!
  var tcl_MutexFinalize: (@convention(c) (UnsafeMutablePointer<Tcl_Mutex?>!) -> Void)!
  var tcl_CreateThread: (@convention(c) (UnsafeMutablePointer<Tcl_ThreadId?>!, (@convention(c) (ClientData!) -> Void)!, ClientData!, Int32, Int32) -> Int32)!
  var tcl_ReadRaw: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<Int8>!, Int32) -> Int32)!
  var tcl_WriteRaw: (@convention(c) (Tcl_Channel!, UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_GetTopChannel: (@convention(c) (Tcl_Channel!) -> Tcl_Channel!)!
  var tcl_ChannelBuffered: (@convention(c) (Tcl_Channel!) -> Int32)!
  var tcl_ChannelName: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> UnsafePointer<Int8>!)!
  var tcl_ChannelVersion: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> Tcl_ChannelTypeVersion!)!
  var tcl_ChannelBlockModeProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32) -> Int32)!)!
  var tcl_ChannelCloseProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!)!
  var tcl_ChannelClose2Proc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!)!
  var tcl_ChannelInputProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!)!
  var tcl_ChannelOutputProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!)!
  var tcl_ChannelSeekProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!)!
  var tcl_ChannelSetOptionProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!)!
  var tcl_ChannelGetOptionProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_DString>!) -> Int32)!)!
  var tcl_ChannelWatchProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32) -> Void)!)!
  var tcl_ChannelGetHandleProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32, UnsafeMutablePointer<ClientData?>!) -> Int32)!)!
  var tcl_ChannelFlushProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!) -> Int32)!)!
  var tcl_ChannelHandlerProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32) -> Int32)!)!
  var tcl_JoinThread: (@convention(c) (Tcl_ThreadId!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_IsChannelShared: (@convention(c) (Tcl_Channel!) -> Int32)!
  var tcl_IsChannelRegistered: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!) -> Int32)!
  var tcl_CutChannel: (@convention(c) (Tcl_Channel!) -> Void)!
  var tcl_SpliceChannel: (@convention(c) (Tcl_Channel!) -> Void)!
  var tcl_ClearChannelHandlers: (@convention(c) (Tcl_Channel!) -> Void)!
  var tcl_IsChannelExisting: (@convention(c) (UnsafePointer<Int8>!) -> Int32)!
  var tcl_UniCharNcasecmp: (@convention(c) (UnsafePointer<Tcl_UniChar>!, UnsafePointer<Tcl_UniChar>!, UInt) -> Int32)!
  var tcl_UniCharCaseMatch: (@convention(c) (UnsafePointer<Tcl_UniChar>!, UnsafePointer<Tcl_UniChar>!, Int32) -> Int32)!
  var tcl_FindHashEntry: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, UnsafePointer<Int8>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)!
  var tcl_CreateHashEntry: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)!
  var tcl_InitCustomHashTable: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, Int32, UnsafeMutablePointer<Tcl_HashKeyType>!) -> Void)!
  var tcl_InitObjHashTable: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!) -> Void)!
  var tcl_CommandTraceInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Void)!, ClientData!) -> ClientData!)!
  var tcl_TraceCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Void)!, ClientData!) -> Int32)!
  var tcl_UntraceCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Void)!, ClientData!) -> Void)!
  var tcl_AttemptAlloc: (@convention(c) (UInt32) -> UnsafeMutablePointer<Int8>!)!
  var tcl_AttemptDbCkalloc: (@convention(c) (UInt32, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!
  var tcl_AttemptRealloc: (@convention(c) (UnsafeMutablePointer<Int8>!, UInt32) -> UnsafeMutablePointer<Int8>!)!
  var tcl_AttemptDbCkrealloc: (@convention(c) (UnsafeMutablePointer<Int8>!, UInt32, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!
  var tcl_AttemptSetObjLength: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Int32)!
  var tcl_GetChannelThread: (@convention(c) (Tcl_Channel!) -> Tcl_ThreadId!)!
  var tcl_GetUnicodeFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Tcl_UniChar>!)!
  var tcl_GetMathFuncInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_ValueType>?>!, UnsafeMutablePointer<(@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Value>!, UnsafeMutablePointer<Tcl_Value>!) -> Int32)?>!, UnsafeMutablePointer<ClientData?>!) -> Int32)!
  var tcl_ListMathFuncs: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_SubstObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_DetachChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!) -> Int32)!
  var tcl_IsStandardChannel: (@convention(c) (Tcl_Channel!) -> Int32)!
  var tcl_FSCopyFile: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_FSCopyDirectory: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var tcl_FSCreateDirectory: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_FSDeleteFile: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_FSLoadFile: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, UnsafeMutablePointer<(@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)?>!, UnsafeMutablePointer<(@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)?>!, UnsafeMutablePointer<Tcl_LoadHandle?>!, UnsafeMutablePointer<(@convention(c) (Tcl_LoadHandle!) -> Void)?>!) -> Int32)!
  var tcl_FSMatchInDirectory: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_GlobTypeData>!) -> Int32)!
  var tcl_FSLink: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_FSRemoveDirectory: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var tcl_FSRenameFile: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_FSLstat: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_StatBuf>!) -> Int32)!
  var tcl_FSUtime: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<utimbuf>!) -> Int32)!
  var tcl_FSFileAttrsGet: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var tcl_FSFileAttrsSet: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_FSFileAttrStrings: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<UnsafePointer<Int8>?>!)!
  var tcl_FSStat: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_StatBuf>!) -> Int32)!
  var tcl_FSAccess: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Int32)!
  var tcl_FSOpenFileChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32) -> Tcl_Channel!)!
  var tcl_FSGetCwd: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_FSChdir: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_FSConvertToPathType: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_FSJoinPath: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_FSSplitPath: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_FSEqualPaths: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_FSGetNormalizedPath: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_FSJoinToPath: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_FSGetInternalRep: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Filesystem>!) -> ClientData!)!
  var tcl_FSGetTranslatedPath: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_FSEvalFile: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_FSNewNativePath: (@convention(c) (UnsafeMutablePointer<Tcl_Filesystem>!, ClientData!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_FSGetNativePath: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafePointer<Int8>!)!
  var tcl_FSFileSystemInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_FSPathSeparator: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_FSListVolumes: (@convention(c) () -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_FSRegister: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Filesystem>!) -> Int32)!
  var tcl_FSUnregister: (@convention(c) (UnsafeMutablePointer<Tcl_Filesystem>!) -> Int32)!
  var tcl_FSData: (@convention(c) (UnsafeMutablePointer<Tcl_Filesystem>!) -> ClientData!)!
  var tcl_FSGetTranslatedStringPath: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> UnsafePointer<Int8>!)!
  var tcl_FSGetFileSystemForPath: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Filesystem>!)!
  var tcl_FSGetPathType: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Tcl_PathType)!
  var tcl_OutputBuffered: (@convention(c) (Tcl_Channel!) -> Int32)!
  var tcl_FSMountsChanged: (@convention(c) (UnsafeMutablePointer<Tcl_Filesystem>!) -> Void)!
  var tcl_EvalTokensStandard: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Token>!, Int32) -> Int32)!
  var tcl_GetTime: (@convention(c) (UnsafeMutablePointer<Tcl_Time>!) -> Void)!
  var tcl_CreateObjTrace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<Int8>!, Tcl_Command!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, ClientData!, (@convention(c) (ClientData!) -> Void)!) -> Tcl_Trace!)!
  var tcl_GetCommandInfoFromToken: (@convention(c) (Tcl_Command!, UnsafeMutablePointer<Tcl_CmdInfo>!) -> Int32)!
  var tcl_SetCommandInfoFromToken: (@convention(c) (Tcl_Command!, UnsafePointer<Tcl_CmdInfo>!) -> Int32)!
  var tcl_DbNewWideIntObj: (@convention(c) (Tcl_WideInt, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_GetWideIntFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_WideInt>!) -> Int32)!
  var tcl_NewWideIntObj: (@convention(c) (Tcl_WideInt) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_SetWideIntObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Tcl_WideInt) -> Void)!
  var tcl_AllocStatBuf: (@convention(c) () -> UnsafeMutablePointer<Tcl_StatBuf>!)!
  var tcl_Seek: (@convention(c) (Tcl_Channel!, Tcl_WideInt, Int32) -> Tcl_WideInt)!
  var tcl_Tell: (@convention(c) (Tcl_Channel!) -> Tcl_WideInt)!
  var tcl_ChannelWideSeekProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Tcl_WideInt, Int32, UnsafeMutablePointer<Int32>!) -> Tcl_WideInt)!)!
  var tcl_DictObjPut: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_DictObjGet: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var tcl_DictObjRemove: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_DictObjSize: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_DictObjFirst: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_DictSearch>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_DictObjNext: (@convention(c) (UnsafeMutablePointer<Tcl_DictSearch>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafeMutablePointer<Int32>!) -> Void)!
  var tcl_DictObjDone: (@convention(c) (UnsafeMutablePointer<Tcl_DictSearch>!) -> Void)!
  var tcl_DictObjPutKeyList: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_DictObjRemoveKeyList: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var tcl_NewDictObj: (@convention(c) () -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_DbNewDictObj: (@convention(c) (UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_RegisterConfig: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_Config>!, UnsafePointer<Int8>!) -> Void)!
  var tcl_CreateNamespace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, ClientData!, (@convention(c) (ClientData!) -> Void)!) -> UnsafeMutablePointer<Tcl_Namespace>!)!
  var tcl_DeleteNamespace: (@convention(c) (UnsafeMutablePointer<Tcl_Namespace>!) -> Void)!
  var tcl_AppendExportList: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Namespace>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_Export: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Namespace>!, UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_Import: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Namespace>!, UnsafePointer<Int8>!, Int32) -> Int32)!
  var tcl_ForgetImport: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Namespace>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_GetCurrentNamespace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Namespace>!)!
  var tcl_GetGlobalNamespace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Namespace>!)!
  var tcl_FindNamespace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_Namespace>!, Int32) -> UnsafeMutablePointer<Tcl_Namespace>!)!
  var tcl_FindCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_Namespace>!, Int32) -> Tcl_Command!)!
  var tcl_GetCommandFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Tcl_Command!)!
  var tcl_GetCommandFullName: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!
  var tcl_FSEvalFileEx: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!) -> Int32)!
  var tcl_SetExitProc: (@convention(c) ((@convention(c) (ClientData!) -> Void)!) -> (@convention(c) (ClientData!) -> Void)!)!
  var tcl_LimitAddHandler: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, ClientData!, (@convention(c) (ClientData!) -> Void)!) -> Void)!
  var tcl_LimitRemoveHandler: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, ClientData!) -> Void)!
  var tcl_LimitReady: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!
  var tcl_LimitCheck: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!
  var tcl_LimitExceeded: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!
  var tcl_LimitSetCommands: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Void)!
  var tcl_LimitSetTime: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Time>!) -> Void)!
  var tcl_LimitSetGranularity: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, Int32) -> Void)!
  var tcl_LimitTypeEnabled: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!
  var tcl_LimitTypeExceeded: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!
  var tcl_LimitTypeSet: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Void)!
  var tcl_LimitTypeReset: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Void)!
  var tcl_LimitGetCommands: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!
  var tcl_LimitGetTime: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Time>!) -> Void)!
  var tcl_LimitGetGranularity: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!
  var tcl_SaveInterpState: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Tcl_InterpState!)!
  var tcl_RestoreInterpState: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_InterpState!) -> Int32)!
  var tcl_DiscardInterpState: (@convention(c) (Tcl_InterpState!) -> Void)!
  var tcl_SetReturnOptions: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_GetReturnOptions: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_IsEnsemble: (@convention(c) (Tcl_Command!) -> Int32)!
  var tcl_CreateEnsemble: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_Namespace>!, Int32) -> Tcl_Command!)!
  var tcl_FindEnsemble: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Tcl_Command!)!
  var tcl_SetEnsembleSubcommandList: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_SetEnsembleMappingDict: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_SetEnsembleUnknownHandler: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_SetEnsembleFlags: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, Int32) -> Int32)!
  var tcl_GetEnsembleSubcommandList: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var tcl_GetEnsembleMappingDict: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var tcl_GetEnsembleUnknownHandler: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var tcl_GetEnsembleFlags: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<Int32>!) -> Int32)!
  var tcl_GetEnsembleNamespace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Namespace>?>!) -> Int32)!
  var tcl_SetTimeProc: (@convention(c) ((@convention(c) (UnsafeMutablePointer<Tcl_Time>!, ClientData!) -> Void)!, (@convention(c) (UnsafeMutablePointer<Tcl_Time>!, ClientData!) -> Void)!, ClientData!) -> Void)!
  var tcl_QueryTimeProc: (@convention(c) (UnsafeMutablePointer<(@convention(c) (UnsafeMutablePointer<Tcl_Time>!, ClientData!) -> Void)?>!, UnsafeMutablePointer<(@convention(c) (UnsafeMutablePointer<Tcl_Time>!, ClientData!) -> Void)?>!, UnsafeMutablePointer<ClientData?>!) -> Void)!
  var tcl_ChannelThreadActionProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32) -> Void)!)!
  var tcl_NewBignumObj: (@convention(c) (UnsafeMutablePointer<mp_int>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_DbNewBignumObj: (@convention(c) (UnsafeMutablePointer<mp_int>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_SetBignumObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<mp_int>!) -> Void)!
  var tcl_GetBignumFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<mp_int>!) -> Int32)!
  var tcl_TakeBignumFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<mp_int>!) -> Int32)!
  var tcl_TruncateChannel: (@convention(c) (Tcl_Channel!, Tcl_WideInt) -> Int32)!
  var tcl_ChannelTruncateProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Tcl_WideInt) -> Int32)!)!
  var tcl_SetChannelErrorInterp: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!
  var tcl_GetChannelErrorInterp: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Void)!
  var tcl_SetChannelError: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!
  var tcl_GetChannelError: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Void)!
  var tcl_InitBignumFromDouble: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Double, UnsafeMutablePointer<mp_int>!) -> Int32)!
  var tcl_GetNamespaceUnknownHandler: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Namespace>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_SetNamespaceUnknownHandler: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Namespace>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_GetEncodingFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Encoding?>!) -> Int32)!
  var tcl_GetEncodingSearchPath: (@convention(c) () -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_SetEncodingSearchPath: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!
  var tcl_GetEncodingNameFromEnvironment: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!) -> UnsafePointer<Int8>!)!
  var tcl_PkgRequireProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafeMutablePointer<ClientData?>!) -> Int32)!
  var tcl_AppendObjToErrorInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!
  var tcl_AppendLimitedToObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32, Int32, UnsafePointer<Int8>!) -> Void)!
  var tcl_Format: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<Tcl_Obj>!)!
  var tcl_AppendFormatToObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!
  var tcl_ObjPrintf: OpaquePointer!
  var tcl_AppendPrintfToObj: OpaquePointer!
  init()
  init(magic magic: Int32, hooks hooks: UnsafeMutablePointer<TclStubHooks>!, tcl_PkgProvideEx tcl_PkgProvideEx: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, ClientData!) -> Int32)!, tcl_PkgRequireEx tcl_PkgRequireEx: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<ClientData?>!) -> UnsafePointer<Int8>!)!, tcl_Panic tcl_Panic: OpaquePointer!, tcl_Alloc tcl_Alloc: (@convention(c) (UInt32) -> UnsafeMutablePointer<Int8>!)!, tcl_Free tcl_Free: (@convention(c) (UnsafeMutablePointer<Int8>!) -> Void)!, tcl_Realloc tcl_Realloc: (@convention(c) (UnsafeMutablePointer<Int8>!, UInt32) -> UnsafeMutablePointer<Int8>!)!, tcl_DbCkalloc tcl_DbCkalloc: (@convention(c) (UInt32, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, tcl_DbCkfree tcl_DbCkfree: (@convention(c) (UnsafeMutablePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_DbCkrealloc tcl_DbCkrealloc: (@convention(c) (UnsafeMutablePointer<Int8>!, UInt32, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, tcl_CreateFileHandler tcl_CreateFileHandler: (@convention(c) (Int32, Int32, (@convention(c) (ClientData!, Int32) -> Void)!, ClientData!) -> Void)!, tcl_DeleteFileHandler tcl_DeleteFileHandler: (@convention(c) (Int32) -> Void)!, tcl_SetTimer tcl_SetTimer: (@convention(c) (UnsafeMutablePointer<Tcl_Time>!) -> Void)!, tcl_Sleep tcl_Sleep: (@convention(c) (Int32) -> Void)!, tcl_WaitForEvent tcl_WaitForEvent: (@convention(c) (UnsafeMutablePointer<Tcl_Time>!) -> Int32)!, tcl_AppendAllObjTypes tcl_AppendAllObjTypes: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_AppendStringsToObj tcl_AppendStringsToObj: OpaquePointer!, tcl_AppendToObj tcl_AppendToObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32) -> Void)!, tcl_ConcatObj tcl_ConcatObj: (@convention(c) (Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_ConvertToType tcl_ConvertToType: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_ObjType>!) -> Int32)!, tcl_DbDecrRefCount tcl_DbDecrRefCount: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32) -> Void)!, tcl_DbIncrRefCount tcl_DbIncrRefCount: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32) -> Void)!, tcl_DbIsShared tcl_DbIsShared: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_DbNewBooleanObj tcl_DbNewBooleanObj: (@convention(c) (Int32, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_DbNewByteArrayObj tcl_DbNewByteArrayObj: (@convention(c) (UnsafePointer<UInt8>!, Int32, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_DbNewDoubleObj tcl_DbNewDoubleObj: (@convention(c) (Double, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_DbNewListObj tcl_DbNewListObj: (@convention(c) (Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_DbNewLongObj tcl_DbNewLongObj: (@convention(c) (Int, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_DbNewObj tcl_DbNewObj: (@convention(c) (UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_DbNewStringObj tcl_DbNewStringObj: (@convention(c) (UnsafePointer<Int8>!, Int32, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_DuplicateObj tcl_DuplicateObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tclFreeObj tclFreeObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Void)!, tcl_GetBoolean tcl_GetBoolean: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_GetBooleanFromObj tcl_GetBooleanFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_GetByteArrayFromObj tcl_GetByteArrayFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<UInt8>!)!, tcl_GetDouble tcl_GetDouble: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Double>!) -> Int32)!, tcl_GetDoubleFromObj tcl_GetDoubleFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Double>!) -> Int32)!, tcl_GetIndexFromObj tcl_GetIndexFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafePointer<Int8>?>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_GetInt tcl_GetInt: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_GetIntFromObj tcl_GetIntFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_GetLongFromObj tcl_GetLongFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int>!) -> Int32)!, tcl_GetObjType tcl_GetObjType: (@convention(c) (UnsafePointer<Int8>!) -> UnsafeMutablePointer<Tcl_ObjType>!)!, tcl_GetStringFromObj tcl_GetStringFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Int8>!)!, tcl_InvalidateStringRep tcl_InvalidateStringRep: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Void)!, tcl_ListObjAppendList tcl_ListObjAppendList: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_ListObjAppendElement tcl_ListObjAppendElement: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_ListObjGetElements tcl_ListObjGetElements: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>?>!) -> Int32)!, tcl_ListObjIndex tcl_ListObjIndex: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, tcl_ListObjLength tcl_ListObjLength: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_ListObjReplace tcl_ListObjReplace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32, Int32, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, tcl_NewBooleanObj tcl_NewBooleanObj: (@convention(c) (Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_NewByteArrayObj tcl_NewByteArrayObj: (@convention(c) (UnsafePointer<UInt8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_NewDoubleObj tcl_NewDoubleObj: (@convention(c) (Double) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_NewIntObj tcl_NewIntObj: (@convention(c) (Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_NewListObj tcl_NewListObj: (@convention(c) (Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_NewLongObj tcl_NewLongObj: (@convention(c) (Int) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_NewObj tcl_NewObj: (@convention(c) () -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_NewStringObj tcl_NewStringObj: (@convention(c) (UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_SetBooleanObj tcl_SetBooleanObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Void)!, tcl_SetByteArrayLength tcl_SetByteArrayLength: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<UInt8>!)!, tcl_SetByteArrayObj tcl_SetByteArrayObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<UInt8>!, Int32) -> Void)!, tcl_SetDoubleObj tcl_SetDoubleObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Double) -> Void)!, tcl_SetIntObj tcl_SetIntObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Void)!, tcl_SetListObj tcl_SetListObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Void)!, tcl_SetLongObj tcl_SetLongObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int) -> Void)!, tcl_SetObjLength tcl_SetObjLength: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Void)!, tcl_SetStringObj tcl_SetStringObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32) -> Void)!, tcl_AddErrorInfo tcl_AddErrorInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Void)!, tcl_AddObjErrorInfo tcl_AddObjErrorInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32) -> Void)!, tcl_AllowExceptions tcl_AllowExceptions: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, tcl_AppendElement tcl_AppendElement: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Void)!, tcl_AppendResult tcl_AppendResult: OpaquePointer!, tcl_AsyncCreate tcl_AsyncCreate: (@convention(c) ((@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!, ClientData!) -> Tcl_AsyncHandler!)!, tcl_AsyncDelete tcl_AsyncDelete: (@convention(c) (Tcl_AsyncHandler!) -> Void)!, tcl_AsyncInvoke tcl_AsyncInvoke: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!, tcl_AsyncMark tcl_AsyncMark: (@convention(c) (Tcl_AsyncHandler!) -> Void)!, tcl_AsyncReady tcl_AsyncReady: (@convention(c) () -> Int32)!, tcl_BackgroundError tcl_BackgroundError: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, tcl_Backslash tcl_Backslash: (@convention(c) (UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> Int8)!, tcl_BadChannelOption tcl_BadChannelOption: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!, tcl_CallWhenDeleted tcl_CallWhenDeleted: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, ClientData!) -> Void)!, tcl_CancelIdleCall tcl_CancelIdleCall: (@convention(c) ((@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!, tcl_Close tcl_Close: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!) -> Int32)!, tcl_CommandComplete tcl_CommandComplete: (@convention(c) (UnsafePointer<Int8>!) -> Int32)!, tcl_Concat tcl_Concat: (@convention(c) (Int32, UnsafePointer<UnsafePointer<Int8>?>!) -> UnsafeMutablePointer<Int8>!)!, tcl_ConvertElement tcl_ConvertElement: (@convention(c) (UnsafePointer<Int8>!, UnsafeMutablePointer<Int8>!, Int32) -> Int32)!, tcl_ConvertCountedElement tcl_ConvertCountedElement: (@convention(c) (UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Int8>!, Int32) -> Int32)!, tcl_CreateAlias tcl_CreateAlias: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafePointer<UnsafePointer<Int8>?>!) -> Int32)!, tcl_CreateAliasObj tcl_CreateAliasObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, tcl_CreateChannel tcl_CreateChannel: (@convention(c) (UnsafeMutablePointer<Tcl_ChannelType>!, UnsafePointer<Int8>!, ClientData!, Int32) -> Tcl_Channel!)!, tcl_CreateChannelHandler tcl_CreateChannelHandler: (@convention(c) (Tcl_Channel!, Int32, (@convention(c) (ClientData!, Int32) -> Void)!, ClientData!) -> Void)!, tcl_CreateCloseHandler tcl_CreateCloseHandler: (@convention(c) (Tcl_Channel!, (@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!, tcl_CreateCommand tcl_CreateCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32)!, ClientData!, (@convention(c) (ClientData!) -> Void)!) -> Tcl_Command!)!, tcl_CreateEventSource tcl_CreateEventSource: (@convention(c) ((@convention(c) (ClientData!, Int32) -> Void)!, (@convention(c) (ClientData!, Int32) -> Void)!, ClientData!) -> Void)!, tcl_CreateExitHandler tcl_CreateExitHandler: (@convention(c) ((@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!, tcl_CreateInterp tcl_CreateInterp: (@convention(c) () -> UnsafeMutablePointer<Tcl_Interp>!)!, tcl_CreateMathFunc tcl_CreateMathFunc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_ValueType>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Value>!, UnsafeMutablePointer<Tcl_Value>!) -> Int32)!, ClientData!) -> Void)!, tcl_CreateObjCommand tcl_CreateObjCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, ClientData!, (@convention(c) (ClientData!) -> Void)!) -> Tcl_Command!)!, tcl_CreateSlave tcl_CreateSlave: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Interp>!)!, tcl_CreateTimerHandler tcl_CreateTimerHandler: (@convention(c) (Int32, (@convention(c) (ClientData!) -> Void)!, ClientData!) -> Tcl_TimerToken!)!, tcl_CreateTrace tcl_CreateTrace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<Int8>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32)!, ClientData!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Void)!, ClientData!) -> Tcl_Trace!)!, tcl_DeleteAssocData tcl_DeleteAssocData: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Void)!, tcl_DeleteChannelHandler tcl_DeleteChannelHandler: (@convention(c) (Tcl_Channel!, (@convention(c) (ClientData!, Int32) -> Void)!, ClientData!) -> Void)!, tcl_DeleteCloseHandler tcl_DeleteCloseHandler: (@convention(c) (Tcl_Channel!, (@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!, tcl_DeleteCommand tcl_DeleteCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Int32)!, tcl_DeleteCommandFromToken tcl_DeleteCommandFromToken: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!) -> Int32)!, tcl_DeleteEvents tcl_DeleteEvents: (@convention(c) ((@convention(c) (UnsafeMutablePointer<Tcl_Event>!, ClientData!) -> Int32)!, ClientData!) -> Void)!, tcl_DeleteEventSource tcl_DeleteEventSource: (@convention(c) ((@convention(c) (ClientData!, Int32) -> Void)!, (@convention(c) (ClientData!, Int32) -> Void)!, ClientData!) -> Void)!, tcl_DeleteExitHandler tcl_DeleteExitHandler: (@convention(c) ((@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!, tcl_DeleteHashEntry tcl_DeleteHashEntry: (@convention(c) (UnsafeMutablePointer<Tcl_HashEntry>!) -> Void)!, tcl_DeleteHashTable tcl_DeleteHashTable: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!) -> Void)!, tcl_DeleteInterp tcl_DeleteInterp: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, tcl_DetachPids tcl_DetachPids: (@convention(c) (Int32, UnsafeMutablePointer<Tcl_Pid?>!) -> Void)!, tcl_DeleteTimerHandler tcl_DeleteTimerHandler: (@convention(c) (Tcl_TimerToken!) -> Void)!, tcl_DeleteTrace tcl_DeleteTrace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Trace!) -> Void)!, tcl_DontCallWhenDeleted tcl_DontCallWhenDeleted: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, ClientData!) -> Void)!, tcl_DoOneEvent tcl_DoOneEvent: (@convention(c) (Int32) -> Int32)!, tcl_DoWhenIdle tcl_DoWhenIdle: (@convention(c) ((@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!, tcl_DStringAppend tcl_DStringAppend: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, tcl_DStringAppendElement tcl_DStringAppendElement: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!, UnsafePointer<Int8>!) -> UnsafeMutablePointer<Int8>!)!, tcl_DStringEndSublist tcl_DStringEndSublist: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!) -> Void)!, tcl_DStringFree tcl_DStringFree: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!) -> Void)!, tcl_DStringGetResult tcl_DStringGetResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_DString>!) -> Void)!, tcl_DStringInit tcl_DStringInit: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!) -> Void)!, tcl_DStringResult tcl_DStringResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_DString>!) -> Void)!, tcl_DStringSetLength tcl_DStringSetLength: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!, Int32) -> Void)!, tcl_DStringStartSublist tcl_DStringStartSublist: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!) -> Void)!, tcl_Eof tcl_Eof: (@convention(c) (Tcl_Channel!) -> Int32)!, tcl_ErrnoId tcl_ErrnoId: (@convention(c) () -> UnsafePointer<Int8>!)!, tcl_ErrnoMsg tcl_ErrnoMsg: (@convention(c) (Int32) -> UnsafePointer<Int8>!)!, tcl_Eval tcl_Eval: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Int32)!, tcl_EvalFile tcl_EvalFile: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Int32)!, tcl_EvalObj tcl_EvalObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_EventuallyFree tcl_EventuallyFree: (@convention(c) (ClientData!, (@convention(c) (UnsafeMutablePointer<Int8>!) -> Void)!) -> Void)!, tcl_Exit tcl_Exit: (@convention(c) (Int32) -> Void)!, tcl_ExposeCommand tcl_ExposeCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!, tcl_ExprBoolean tcl_ExprBoolean: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_ExprBooleanObj tcl_ExprBooleanObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_ExprDouble tcl_ExprDouble: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Double>!) -> Int32)!, tcl_ExprDoubleObj tcl_ExprDoubleObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Double>!) -> Int32)!, tcl_ExprLong tcl_ExprLong: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int>!) -> Int32)!, tcl_ExprLongObj tcl_ExprLongObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int>!) -> Int32)!, tcl_ExprObj tcl_ExprObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, tcl_ExprString tcl_ExprString: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Int32)!, tcl_Finalize tcl_Finalize: (@convention(c) () -> Void)!, tcl_FindExecutable tcl_FindExecutable: (@convention(c) (UnsafePointer<Int8>!) -> Void)!, tcl_FirstHashEntry tcl_FirstHashEntry: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, UnsafeMutablePointer<Tcl_HashSearch>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)!, tcl_Flush tcl_Flush: (@convention(c) (Tcl_Channel!) -> Int32)!, tcl_FreeResult tcl_FreeResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, tcl_GetAlias tcl_GetAlias: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Interp>?>!, UnsafeMutablePointer<UnsafePointer<Int8>?>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<UnsafeMutablePointer<UnsafePointer<Int8>?>?>!) -> Int32)!, tcl_GetAliasObj tcl_GetAliasObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Interp>?>!, UnsafeMutablePointer<UnsafePointer<Int8>?>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>?>!) -> Int32)!, tcl_GetAssocData tcl_GetAssocData: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<(@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)?>!) -> ClientData!)!, tcl_GetChannel tcl_GetChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> Tcl_Channel!)!, tcl_GetChannelBufferSize tcl_GetChannelBufferSize: (@convention(c) (Tcl_Channel!) -> Int32)!, tcl_GetChannelHandle tcl_GetChannelHandle: (@convention(c) (Tcl_Channel!, Int32, UnsafeMutablePointer<ClientData?>!) -> Int32)!, tcl_GetChannelInstanceData tcl_GetChannelInstanceData: (@convention(c) (Tcl_Channel!) -> ClientData!)!, tcl_GetChannelMode tcl_GetChannelMode: (@convention(c) (Tcl_Channel!) -> Int32)!, tcl_GetChannelName tcl_GetChannelName: (@convention(c) (Tcl_Channel!) -> UnsafePointer<Int8>!)!, tcl_GetChannelOption tcl_GetChannelOption: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_DString>!) -> Int32)!, tcl_GetChannelType tcl_GetChannelType: (@convention(c) (Tcl_Channel!) -> UnsafeMutablePointer<Tcl_ChannelType>!)!, tcl_GetCommandInfo tcl_GetCommandInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_CmdInfo>!) -> Int32)!, tcl_GetCommandName tcl_GetCommandName: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!) -> UnsafePointer<Int8>!)!, tcl_GetErrno tcl_GetErrno: (@convention(c) () -> Int32)!, tcl_GetHostName tcl_GetHostName: (@convention(c) () -> UnsafePointer<Int8>!)!, tcl_GetInterpPath tcl_GetInterpPath: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!, tcl_GetMaster tcl_GetMaster: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Interp>!)!, tcl_GetNameOfExecutable tcl_GetNameOfExecutable: (@convention(c) () -> UnsafePointer<Int8>!)!, tcl_GetObjResult tcl_GetObjResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_GetOpenFile tcl_GetOpenFile: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, Int32, UnsafeMutablePointer<ClientData?>!) -> Int32)!, tcl_GetPathType tcl_GetPathType: (@convention(c) (UnsafePointer<Int8>!) -> Tcl_PathType)!, tcl_Gets tcl_Gets: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<Tcl_DString>!) -> Int32)!, tcl_GetsObj tcl_GetsObj: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_GetServiceMode tcl_GetServiceMode: (@convention(c) () -> Int32)!, tcl_GetSlave tcl_GetSlave: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> UnsafeMutablePointer<Tcl_Interp>!)!, tcl_GetStdChannel tcl_GetStdChannel: (@convention(c) (Int32) -> Tcl_Channel!)!, tcl_GetStringResult tcl_GetStringResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafePointer<Int8>!)!, tcl_GetVar tcl_GetVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!, tcl_GetVar2 tcl_GetVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!, tcl_GlobalEval tcl_GlobalEval: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Int32)!, tcl_GlobalEvalObj tcl_GlobalEvalObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_HideCommand tcl_HideCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!, tcl_Init tcl_Init: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!, tcl_InitHashTable tcl_InitHashTable: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, Int32) -> Void)!, tcl_InputBlocked tcl_InputBlocked: (@convention(c) (Tcl_Channel!) -> Int32)!, tcl_InputBuffered tcl_InputBuffered: (@convention(c) (Tcl_Channel!) -> Int32)!, tcl_InterpDeleted tcl_InterpDeleted: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!, tcl_IsSafe tcl_IsSafe: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!, tcl_JoinPath tcl_JoinPath: (@convention(c) (Int32, UnsafePointer<UnsafePointer<Int8>?>!, UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!)!, tcl_LinkVar tcl_LinkVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int8>!, Int32) -> Int32)!, reserved188 reserved188: UnsafeMutablePointer<Void>!, tcl_MakeFileChannel tcl_MakeFileChannel: (@convention(c) (ClientData!, Int32) -> Tcl_Channel!)!, tcl_MakeSafe tcl_MakeSafe: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!, tcl_MakeTcpClientChannel tcl_MakeTcpClientChannel: (@convention(c) (ClientData!) -> Tcl_Channel!)!, tcl_Merge tcl_Merge: (@convention(c) (Int32, UnsafePointer<UnsafePointer<Int8>?>!) -> UnsafeMutablePointer<Int8>!)!, tcl_NextHashEntry tcl_NextHashEntry: (@convention(c) (UnsafeMutablePointer<Tcl_HashSearch>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)!, tcl_NotifyChannel tcl_NotifyChannel: (@convention(c) (Tcl_Channel!, Int32) -> Void)!, tcl_ObjGetVar2 tcl_ObjGetVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_ObjSetVar2 tcl_ObjSetVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_OpenCommandChannel tcl_OpenCommandChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!, Int32) -> Tcl_Channel!)!, tcl_OpenFileChannel tcl_OpenFileChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Tcl_Channel!)!, tcl_OpenTcpClient tcl_OpenTcpClient: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32, Int32) -> Tcl_Channel!)!, tcl_OpenTcpServer tcl_OpenTcpServer: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<Int8>!, (@convention(c) (ClientData!, Tcl_Channel!, UnsafeMutablePointer<Int8>!, Int32) -> Void)!, ClientData!) -> Tcl_Channel!)!, tcl_Preserve tcl_Preserve: (@convention(c) (ClientData!) -> Void)!, tcl_PrintDouble tcl_PrintDouble: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Double, UnsafeMutablePointer<Int8>!) -> Void)!, tcl_PutEnv tcl_PutEnv: (@convention(c) (UnsafePointer<Int8>!) -> Int32)!, tcl_PosixError tcl_PosixError: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafePointer<Int8>!)!, tcl_QueueEvent tcl_QueueEvent: (@convention(c) (UnsafeMutablePointer<Tcl_Event>!, Tcl_QueuePosition) -> Void)!, tcl_Read tcl_Read: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<Int8>!, Int32) -> Int32)!, tcl_ReapDetachedProcs tcl_ReapDetachedProcs: (@convention(c) () -> Void)!, tcl_RecordAndEval tcl_RecordAndEval: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_RecordAndEvalObj tcl_RecordAndEvalObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Int32)!, tcl_RegisterChannel tcl_RegisterChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!) -> Void)!, tcl_RegisterObjType tcl_RegisterObjType: (@convention(c) (UnsafeMutablePointer<Tcl_ObjType>!) -> Void)!, tcl_RegExpCompile tcl_RegExpCompile: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Tcl_RegExp!)!, tcl_RegExpExec tcl_RegExpExec: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_RegExp!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!, tcl_RegExpMatch tcl_RegExpMatch: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!, tcl_RegExpRange tcl_RegExpRange: (@convention(c) (Tcl_RegExp!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Void)!, tcl_Release tcl_Release: (@convention(c) (ClientData!) -> Void)!, tcl_ResetResult tcl_ResetResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, tcl_ScanElement tcl_ScanElement: (@convention(c) (UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_ScanCountedElement tcl_ScanCountedElement: (@convention(c) (UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_SeekOld tcl_SeekOld: (@convention(c) (Tcl_Channel!, Int32, Int32) -> Int32)!, tcl_ServiceAll tcl_ServiceAll: (@convention(c) () -> Int32)!, tcl_ServiceEvent tcl_ServiceEvent: (@convention(c) (Int32) -> Int32)!, tcl_SetAssocData tcl_SetAssocData: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, ClientData!) -> Void)!, tcl_SetChannelBufferSize tcl_SetChannelBufferSize: (@convention(c) (Tcl_Channel!, Int32) -> Void)!, tcl_SetChannelOption tcl_SetChannelOption: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!, tcl_SetCommandInfo tcl_SetCommandInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Tcl_CmdInfo>!) -> Int32)!, tcl_SetErrno tcl_SetErrno: (@convention(c) (Int32) -> Void)!, tcl_SetErrorCode tcl_SetErrorCode: OpaquePointer!, tcl_SetMaxBlockTime tcl_SetMaxBlockTime: (@convention(c) (UnsafeMutablePointer<Tcl_Time>!) -> Void)!, tcl_SetPanicProc tcl_SetPanicProc: (@convention(c) (OpaquePointer!) -> Void)!, tcl_SetRecursionLimit tcl_SetRecursionLimit: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!, tcl_SetResult tcl_SetResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Int8>!, (@convention(c) (UnsafeMutablePointer<Int8>!) -> Void)!) -> Void)!, tcl_SetServiceMode tcl_SetServiceMode: (@convention(c) (Int32) -> Int32)!, tcl_SetObjErrorCode tcl_SetObjErrorCode: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!, tcl_SetObjResult tcl_SetObjResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!, tcl_SetStdChannel tcl_SetStdChannel: (@convention(c) (Tcl_Channel!, Int32) -> Void)!, tcl_SetVar tcl_SetVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!, tcl_SetVar2 tcl_SetVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!, tcl_SignalId tcl_SignalId: (@convention(c) (Int32) -> UnsafePointer<Int8>!)!, tcl_SignalMsg tcl_SignalMsg: (@convention(c) (Int32) -> UnsafePointer<Int8>!)!, tcl_SourceRCFile tcl_SourceRCFile: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, tcl_SplitList tcl_SplitList: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<UnsafeMutablePointer<UnsafePointer<Int8>?>?>!) -> Int32)!, tcl_SplitPath tcl_SplitPath: (@convention(c) (UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<UnsafeMutablePointer<UnsafePointer<Int8>?>?>!) -> Void)!, tcl_StaticPackage tcl_StaticPackage: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!, (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!) -> Void)!, tcl_StringMatch tcl_StringMatch: (@convention(c) (UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!, tcl_TellOld tcl_TellOld: (@convention(c) (Tcl_Channel!) -> Int32)!, tcl_TraceVar tcl_TraceVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, ClientData!) -> Int32)!, tcl_TraceVar2 tcl_TraceVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, ClientData!) -> Int32)!, tcl_TranslateFileName tcl_TranslateFileName: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!)!, tcl_Ungets tcl_Ungets: (@convention(c) (Tcl_Channel!, UnsafePointer<Int8>!, Int32, Int32) -> Int32)!, tcl_UnlinkVar tcl_UnlinkVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Void)!, tcl_UnregisterChannel tcl_UnregisterChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!) -> Int32)!, tcl_UnsetVar tcl_UnsetVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_UnsetVar2 tcl_UnsetVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_UntraceVar tcl_UntraceVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, ClientData!) -> Void)!, tcl_UntraceVar2 tcl_UntraceVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, ClientData!) -> Void)!, tcl_UpdateLinkedVar tcl_UpdateLinkedVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Void)!, tcl_UpVar tcl_UpVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_UpVar2 tcl_UpVar2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_VarEval tcl_VarEval: OpaquePointer!, tcl_VarTraceInfo tcl_VarTraceInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, ClientData!) -> ClientData!)!, tcl_VarTraceInfo2 tcl_VarTraceInfo2: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, ClientData!) -> ClientData!)!, tcl_Write tcl_Write: (@convention(c) (Tcl_Channel!, UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_WrongNumArgs tcl_WrongNumArgs: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafePointer<Int8>!) -> Void)!, tcl_DumpActiveMemory tcl_DumpActiveMemory: (@convention(c) (UnsafePointer<Int8>!) -> Int32)!, tcl_ValidateAllMemory tcl_ValidateAllMemory: (@convention(c) (UnsafePointer<Int8>!, Int32) -> Void)!, tcl_AppendResultVA tcl_AppendResultVA: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, CVaListPointer) -> Void)!, tcl_AppendStringsToObjVA tcl_AppendStringsToObjVA: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, CVaListPointer) -> Void)!, tcl_HashStats tcl_HashStats: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!) -> UnsafeMutablePointer<Int8>!)!, tcl_ParseVar tcl_ParseVar: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> UnsafePointer<Int8>!)!, tcl_PkgPresent tcl_PkgPresent: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!, tcl_PkgPresentEx tcl_PkgPresentEx: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<ClientData?>!) -> UnsafePointer<Int8>!)!, tcl_PkgProvide tcl_PkgProvide: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!, tcl_PkgRequire tcl_PkgRequire: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!, tcl_SetErrorCodeVA tcl_SetErrorCodeVA: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, CVaListPointer) -> Void)!, tcl_VarEvalVA tcl_VarEvalVA: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, CVaListPointer) -> Int32)!, tcl_WaitPid tcl_WaitPid: (@convention(c) (Tcl_Pid!, UnsafeMutablePointer<Int32>!, Int32) -> Tcl_Pid!)!, tcl_PanicVA tcl_PanicVA: (@convention(c) (UnsafePointer<Int8>!, CVaListPointer) -> Void)!, tcl_GetVersion tcl_GetVersion: (@convention(c) (UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!) -> Void)!, tcl_InitMemory tcl_InitMemory: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, tcl_StackChannel tcl_StackChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_ChannelType>!, ClientData!, Int32, Tcl_Channel!) -> Tcl_Channel!)!, tcl_UnstackChannel tcl_UnstackChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!) -> Int32)!, tcl_GetStackedChannel tcl_GetStackedChannel: (@convention(c) (Tcl_Channel!) -> Tcl_Channel!)!, tcl_SetMainLoop tcl_SetMainLoop: (@convention(c) ((@convention(c) () -> Void)!) -> Void)!, reserved285 reserved285: UnsafeMutablePointer<Void>!, tcl_AppendObjToObj tcl_AppendObjToObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!, tcl_CreateEncoding tcl_CreateEncoding: (@convention(c) (UnsafePointer<Tcl_EncodingType>!) -> Tcl_Encoding!)!, tcl_CreateThreadExitHandler tcl_CreateThreadExitHandler: (@convention(c) ((@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!, tcl_DeleteThreadExitHandler tcl_DeleteThreadExitHandler: (@convention(c) ((@convention(c) (ClientData!) -> Void)!, ClientData!) -> Void)!, tcl_DiscardResult tcl_DiscardResult: (@convention(c) (UnsafeMutablePointer<Tcl_SavedResult>!) -> Void)!, tcl_EvalEx tcl_EvalEx: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, Int32) -> Int32)!, tcl_EvalObjv tcl_EvalObjv: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, Int32) -> Int32)!, tcl_EvalObjEx tcl_EvalObjEx: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Int32)!, tcl_ExitThread tcl_ExitThread: (@convention(c) (Int32) -> Void)!, tcl_ExternalToUtf tcl_ExternalToUtf: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Encoding!, UnsafePointer<Int8>!, Int32, Int32, UnsafeMutablePointer<Tcl_EncodingState?>!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_ExternalToUtfDString tcl_ExternalToUtfDString: (@convention(c) (Tcl_Encoding!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!)!, tcl_FinalizeThread tcl_FinalizeThread: (@convention(c) () -> Void)!, tcl_FinalizeNotifier tcl_FinalizeNotifier: (@convention(c) (ClientData!) -> Void)!, tcl_FreeEncoding tcl_FreeEncoding: (@convention(c) (Tcl_Encoding!) -> Void)!, tcl_GetCurrentThread tcl_GetCurrentThread: (@convention(c) () -> Tcl_ThreadId!)!, tcl_GetEncoding tcl_GetEncoding: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Tcl_Encoding!)!, tcl_GetEncodingName tcl_GetEncodingName: (@convention(c) (Tcl_Encoding!) -> UnsafePointer<Int8>!)!, tcl_GetEncodingNames tcl_GetEncodingNames: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, tcl_GetIndexFromObjStruct tcl_GetIndexFromObjStruct: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Void>!, Int32, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_GetThreadData tcl_GetThreadData: (@convention(c) (UnsafeMutablePointer<Tcl_ThreadDataKey?>!, Int32) -> UnsafeMutablePointer<Void>!)!, tcl_GetVar2Ex tcl_GetVar2Ex: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_InitNotifier tcl_InitNotifier: (@convention(c) () -> ClientData!)!, tcl_MutexLock tcl_MutexLock: (@convention(c) (UnsafeMutablePointer<Tcl_Mutex?>!) -> Void)!, tcl_MutexUnlock tcl_MutexUnlock: (@convention(c) (UnsafeMutablePointer<Tcl_Mutex?>!) -> Void)!, tcl_ConditionNotify tcl_ConditionNotify: (@convention(c) (UnsafeMutablePointer<Tcl_Condition?>!) -> Void)!, tcl_ConditionWait tcl_ConditionWait: (@convention(c) (UnsafeMutablePointer<Tcl_Condition?>!, UnsafeMutablePointer<Tcl_Mutex?>!, UnsafeMutablePointer<Tcl_Time>!) -> Void)!, tcl_NumUtfChars tcl_NumUtfChars: (@convention(c) (UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_ReadChars tcl_ReadChars: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<Tcl_Obj>!, Int32, Int32) -> Int32)!, tcl_RestoreResult tcl_RestoreResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_SavedResult>!) -> Void)!, tcl_SaveResult tcl_SaveResult: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_SavedResult>!) -> Void)!, tcl_SetSystemEncoding tcl_SetSystemEncoding: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Int32)!, tcl_SetVar2Ex tcl_SetVar2Ex: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_ThreadAlert tcl_ThreadAlert: (@convention(c) (Tcl_ThreadId!) -> Void)!, tcl_ThreadQueueEvent tcl_ThreadQueueEvent: (@convention(c) (Tcl_ThreadId!, UnsafeMutablePointer<Tcl_Event>!, Tcl_QueuePosition) -> Void)!, tcl_UniCharAtIndex tcl_UniCharAtIndex: (@convention(c) (UnsafePointer<Int8>!, Int32) -> Tcl_UniChar)!, tcl_UniCharToLower tcl_UniCharToLower: (@convention(c) (Int32) -> Tcl_UniChar)!, tcl_UniCharToTitle tcl_UniCharToTitle: (@convention(c) (Int32) -> Tcl_UniChar)!, tcl_UniCharToUpper tcl_UniCharToUpper: (@convention(c) (Int32) -> Tcl_UniChar)!, tcl_UniCharToUtf tcl_UniCharToUtf: (@convention(c) (Int32, UnsafeMutablePointer<Int8>!) -> Int32)!, tcl_UtfAtIndex tcl_UtfAtIndex: (@convention(c) (UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!, tcl_UtfCharComplete tcl_UtfCharComplete: (@convention(c) (UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_UtfBackslash tcl_UtfBackslash: (@convention(c) (UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int8>!) -> Int32)!, tcl_UtfFindFirst tcl_UtfFindFirst: (@convention(c) (UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!, tcl_UtfFindLast tcl_UtfFindLast: (@convention(c) (UnsafePointer<Int8>!, Int32) -> UnsafePointer<Int8>!)!, tcl_UtfNext tcl_UtfNext: (@convention(c) (UnsafePointer<Int8>!) -> UnsafePointer<Int8>!)!, tcl_UtfPrev tcl_UtfPrev: (@convention(c) (UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> UnsafePointer<Int8>!)!, tcl_UtfToExternal tcl_UtfToExternal: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Encoding!, UnsafePointer<Int8>!, Int32, Int32, UnsafeMutablePointer<Tcl_EncodingState?>!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_UtfToExternalDString tcl_UtfToExternalDString: (@convention(c) (Tcl_Encoding!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!)!, tcl_UtfToLower tcl_UtfToLower: (@convention(c) (UnsafeMutablePointer<Int8>!) -> Int32)!, tcl_UtfToTitle tcl_UtfToTitle: (@convention(c) (UnsafeMutablePointer<Int8>!) -> Int32)!, tcl_UtfToUniChar tcl_UtfToUniChar: (@convention(c) (UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_UniChar>!) -> Int32)!, tcl_UtfToUpper tcl_UtfToUpper: (@convention(c) (UnsafeMutablePointer<Int8>!) -> Int32)!, tcl_WriteChars tcl_WriteChars: (@convention(c) (Tcl_Channel!, UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_WriteObj tcl_WriteObj: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_GetString tcl_GetString: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Int8>!)!, tcl_GetDefaultEncodingDir tcl_GetDefaultEncodingDir: (@convention(c) () -> UnsafePointer<Int8>!)!, tcl_SetDefaultEncodingDir tcl_SetDefaultEncodingDir: (@convention(c) (UnsafePointer<Int8>!) -> Void)!, tcl_AlertNotifier tcl_AlertNotifier: (@convention(c) (ClientData!) -> Void)!, tcl_ServiceModeHook tcl_ServiceModeHook: (@convention(c) (Int32) -> Void)!, tcl_UniCharIsAlnum tcl_UniCharIsAlnum: (@convention(c) (Int32) -> Int32)!, tcl_UniCharIsAlpha tcl_UniCharIsAlpha: (@convention(c) (Int32) -> Int32)!, tcl_UniCharIsDigit tcl_UniCharIsDigit: (@convention(c) (Int32) -> Int32)!, tcl_UniCharIsLower tcl_UniCharIsLower: (@convention(c) (Int32) -> Int32)!, tcl_UniCharIsSpace tcl_UniCharIsSpace: (@convention(c) (Int32) -> Int32)!, tcl_UniCharIsUpper tcl_UniCharIsUpper: (@convention(c) (Int32) -> Int32)!, tcl_UniCharIsWordChar tcl_UniCharIsWordChar: (@convention(c) (Int32) -> Int32)!, tcl_UniCharLen tcl_UniCharLen: (@convention(c) (UnsafePointer<Tcl_UniChar>!) -> Int32)!, tcl_UniCharNcmp tcl_UniCharNcmp: (@convention(c) (UnsafePointer<Tcl_UniChar>!, UnsafePointer<Tcl_UniChar>!, UInt) -> Int32)!, tcl_UniCharToUtfDString tcl_UniCharToUtfDString: (@convention(c) (UnsafePointer<Tcl_UniChar>!, Int32, UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!)!, tcl_UtfToUniCharDString tcl_UtfToUniCharDString: (@convention(c) (UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Tcl_UniChar>!)!, tcl_GetRegExpFromObj tcl_GetRegExpFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Tcl_RegExp!)!, tcl_EvalTokens tcl_EvalTokens: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Token>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_FreeParse tcl_FreeParse: (@convention(c) (UnsafeMutablePointer<Tcl_Parse>!) -> Void)!, tcl_LogCommandInfo tcl_LogCommandInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Void)!, tcl_ParseBraces tcl_ParseBraces: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_Parse>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32)!, tcl_ParseCommand tcl_ParseCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, Int32, UnsafeMutablePointer<Tcl_Parse>!) -> Int32)!, tcl_ParseExpr tcl_ParseExpr: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_Parse>!) -> Int32)!, tcl_ParseQuotedString tcl_ParseQuotedString: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_Parse>!, Int32, UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32)!, tcl_ParseVarName tcl_ParseVarName: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Tcl_Parse>!, Int32) -> Int32)!, tcl_GetCwd tcl_GetCwd: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_DString>!) -> UnsafeMutablePointer<Int8>!)!, tcl_Chdir tcl_Chdir: (@convention(c) (UnsafePointer<Int8>!) -> Int32)!, tcl_Access tcl_Access: (@convention(c) (UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_Stat tcl_Stat: (@convention(c) (UnsafePointer<Int8>!, UnsafeMutablePointer<stat>!) -> Int32)!, tcl_UtfNcmp tcl_UtfNcmp: (@convention(c) (UnsafePointer<Int8>!, UnsafePointer<Int8>!, UInt) -> Int32)!, tcl_UtfNcasecmp tcl_UtfNcasecmp: (@convention(c) (UnsafePointer<Int8>!, UnsafePointer<Int8>!, UInt) -> Int32)!, tcl_StringCaseMatch tcl_StringCaseMatch: (@convention(c) (UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_UniCharIsControl tcl_UniCharIsControl: (@convention(c) (Int32) -> Int32)!, tcl_UniCharIsGraph tcl_UniCharIsGraph: (@convention(c) (Int32) -> Int32)!, tcl_UniCharIsPrint tcl_UniCharIsPrint: (@convention(c) (Int32) -> Int32)!, tcl_UniCharIsPunct tcl_UniCharIsPunct: (@convention(c) (Int32) -> Int32)!, tcl_RegExpExecObj tcl_RegExpExecObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_RegExp!, UnsafeMutablePointer<Tcl_Obj>!, Int32, Int32, Int32) -> Int32)!, tcl_RegExpGetInfo tcl_RegExpGetInfo: (@convention(c) (Tcl_RegExp!, UnsafeMutablePointer<Tcl_RegExpInfo>!) -> Void)!, tcl_NewUnicodeObj tcl_NewUnicodeObj: (@convention(c) (UnsafePointer<Tcl_UniChar>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_SetUnicodeObj tcl_SetUnicodeObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Tcl_UniChar>!, Int32) -> Void)!, tcl_GetCharLength tcl_GetCharLength: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_GetUniChar tcl_GetUniChar: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Tcl_UniChar)!, tcl_GetUnicode tcl_GetUnicode: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_UniChar>!)!, tcl_GetRange tcl_GetRange: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_AppendUnicodeToObj tcl_AppendUnicodeToObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Tcl_UniChar>!, Int32) -> Void)!, tcl_RegExpMatchObj tcl_RegExpMatchObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_SetNotifier tcl_SetNotifier: (@convention(c) (UnsafeMutablePointer<Tcl_NotifierProcs>!) -> Void)!, tcl_GetAllocMutex tcl_GetAllocMutex: (@convention(c) () -> UnsafeMutablePointer<Tcl_Mutex?>!)!, tcl_GetChannelNames tcl_GetChannelNames: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!, tcl_GetChannelNamesEx tcl_GetChannelNamesEx: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> Int32)!, tcl_ProcObjCmd tcl_ProcObjCmd: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, tcl_ConditionFinalize tcl_ConditionFinalize: (@convention(c) (UnsafeMutablePointer<Tcl_Condition?>!) -> Void)!, tcl_MutexFinalize tcl_MutexFinalize: (@convention(c) (UnsafeMutablePointer<Tcl_Mutex?>!) -> Void)!, tcl_CreateThread tcl_CreateThread: (@convention(c) (UnsafeMutablePointer<Tcl_ThreadId?>!, (@convention(c) (ClientData!) -> Void)!, ClientData!, Int32, Int32) -> Int32)!, tcl_ReadRaw tcl_ReadRaw: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<Int8>!, Int32) -> Int32)!, tcl_WriteRaw tcl_WriteRaw: (@convention(c) (Tcl_Channel!, UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_GetTopChannel tcl_GetTopChannel: (@convention(c) (Tcl_Channel!) -> Tcl_Channel!)!, tcl_ChannelBuffered tcl_ChannelBuffered: (@convention(c) (Tcl_Channel!) -> Int32)!, tcl_ChannelName tcl_ChannelName: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> UnsafePointer<Int8>!)!, tcl_ChannelVersion tcl_ChannelVersion: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> Tcl_ChannelTypeVersion!)!, tcl_ChannelBlockModeProc tcl_ChannelBlockModeProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32) -> Int32)!)!, tcl_ChannelCloseProc tcl_ChannelCloseProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!)!, tcl_ChannelClose2Proc tcl_ChannelClose2Proc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!)!, tcl_ChannelInputProc tcl_ChannelInputProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!)!, tcl_ChannelOutputProc tcl_ChannelOutputProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafePointer<Int8>!, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!)!, tcl_ChannelSeekProc tcl_ChannelSeekProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int, Int32, UnsafeMutablePointer<Int32>!) -> Int32)!)!, tcl_ChannelSetOptionProc tcl_ChannelSetOptionProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!) -> Int32)!)!, tcl_ChannelGetOptionProc tcl_ChannelGetOptionProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_DString>!) -> Int32)!)!, tcl_ChannelWatchProc tcl_ChannelWatchProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32) -> Void)!)!, tcl_ChannelGetHandleProc tcl_ChannelGetHandleProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32, UnsafeMutablePointer<ClientData?>!) -> Int32)!)!, tcl_ChannelFlushProc tcl_ChannelFlushProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!) -> Int32)!)!, tcl_ChannelHandlerProc tcl_ChannelHandlerProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32) -> Int32)!)!, tcl_JoinThread tcl_JoinThread: (@convention(c) (Tcl_ThreadId!, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_IsChannelShared tcl_IsChannelShared: (@convention(c) (Tcl_Channel!) -> Int32)!, tcl_IsChannelRegistered tcl_IsChannelRegistered: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!) -> Int32)!, tcl_CutChannel tcl_CutChannel: (@convention(c) (Tcl_Channel!) -> Void)!, tcl_SpliceChannel tcl_SpliceChannel: (@convention(c) (Tcl_Channel!) -> Void)!, tcl_ClearChannelHandlers tcl_ClearChannelHandlers: (@convention(c) (Tcl_Channel!) -> Void)!, tcl_IsChannelExisting tcl_IsChannelExisting: (@convention(c) (UnsafePointer<Int8>!) -> Int32)!, tcl_UniCharNcasecmp tcl_UniCharNcasecmp: (@convention(c) (UnsafePointer<Tcl_UniChar>!, UnsafePointer<Tcl_UniChar>!, UInt) -> Int32)!, tcl_UniCharCaseMatch tcl_UniCharCaseMatch: (@convention(c) (UnsafePointer<Tcl_UniChar>!, UnsafePointer<Tcl_UniChar>!, Int32) -> Int32)!, tcl_FindHashEntry tcl_FindHashEntry: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, UnsafePointer<Int8>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)!, tcl_CreateHashEntry tcl_CreateHashEntry: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Tcl_HashEntry>!)!, tcl_InitCustomHashTable tcl_InitCustomHashTable: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!, Int32, UnsafeMutablePointer<Tcl_HashKeyType>!) -> Void)!, tcl_InitObjHashTable tcl_InitObjHashTable: (@convention(c) (UnsafeMutablePointer<Tcl_HashTable>!) -> Void)!, tcl_CommandTraceInfo tcl_CommandTraceInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Void)!, ClientData!) -> ClientData!)!, tcl_TraceCommand tcl_TraceCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Void)!, ClientData!) -> Int32)!, tcl_UntraceCommand tcl_UntraceCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, Int32) -> Void)!, ClientData!) -> Void)!, tcl_AttemptAlloc tcl_AttemptAlloc: (@convention(c) (UInt32) -> UnsafeMutablePointer<Int8>!)!, tcl_AttemptDbCkalloc tcl_AttemptDbCkalloc: (@convention(c) (UInt32, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, tcl_AttemptRealloc tcl_AttemptRealloc: (@convention(c) (UnsafeMutablePointer<Int8>!, UInt32) -> UnsafeMutablePointer<Int8>!)!, tcl_AttemptDbCkrealloc tcl_AttemptDbCkrealloc: (@convention(c) (UnsafeMutablePointer<Int8>!, UInt32, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Int8>!)!, tcl_AttemptSetObjLength tcl_AttemptSetObjLength: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Int32)!, tcl_GetChannelThread tcl_GetChannelThread: (@convention(c) (Tcl_Channel!) -> Tcl_ThreadId!)!, tcl_GetUnicodeFromObj tcl_GetUnicodeFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Tcl_UniChar>!)!, tcl_GetMathFuncInfo tcl_GetMathFuncInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Int32>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_ValueType>?>!, UnsafeMutablePointer<(@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Value>!, UnsafeMutablePointer<Tcl_Value>!) -> Int32)?>!, UnsafeMutablePointer<ClientData?>!) -> Int32)!, tcl_ListMathFuncs tcl_ListMathFuncs: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_SubstObj tcl_SubstObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_DetachChannel tcl_DetachChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Channel!) -> Int32)!, tcl_IsStandardChannel tcl_IsStandardChannel: (@convention(c) (Tcl_Channel!) -> Int32)!, tcl_FSCopyFile tcl_FSCopyFile: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_FSCopyDirectory tcl_FSCopyDirectory: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, tcl_FSCreateDirectory tcl_FSCreateDirectory: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_FSDeleteFile tcl_FSDeleteFile: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_FSLoadFile tcl_FSLoadFile: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, UnsafePointer<Int8>!, UnsafeMutablePointer<(@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)?>!, UnsafeMutablePointer<(@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)?>!, UnsafeMutablePointer<Tcl_LoadHandle?>!, UnsafeMutablePointer<(@convention(c) (Tcl_LoadHandle!) -> Void)?>!) -> Int32)!, tcl_FSMatchInDirectory tcl_FSMatchInDirectory: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_GlobTypeData>!) -> Int32)!, tcl_FSLink tcl_FSLink: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_FSRemoveDirectory tcl_FSRemoveDirectory: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, tcl_FSRenameFile tcl_FSRenameFile: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_FSLstat tcl_FSLstat: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_StatBuf>!) -> Int32)!, tcl_FSUtime tcl_FSUtime: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<utimbuf>!) -> Int32)!, tcl_FSFileAttrsGet tcl_FSFileAttrsGet: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, tcl_FSFileAttrsSet tcl_FSFileAttrsSet: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_FSFileAttrStrings tcl_FSFileAttrStrings: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<UnsafePointer<Int8>?>!)!, tcl_FSStat tcl_FSStat: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_StatBuf>!) -> Int32)!, tcl_FSAccess tcl_FSAccess: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Int32)!, tcl_FSOpenFileChannel tcl_FSOpenFileChannel: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32) -> Tcl_Channel!)!, tcl_FSGetCwd tcl_FSGetCwd: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_FSChdir tcl_FSChdir: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_FSConvertToPathType tcl_FSConvertToPathType: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_FSJoinPath tcl_FSJoinPath: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_FSSplitPath tcl_FSSplitPath: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_FSEqualPaths tcl_FSEqualPaths: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_FSGetNormalizedPath tcl_FSGetNormalizedPath: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_FSJoinToPath tcl_FSJoinToPath: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_FSGetInternalRep tcl_FSGetInternalRep: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Filesystem>!) -> ClientData!)!, tcl_FSGetTranslatedPath tcl_FSGetTranslatedPath: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_FSEvalFile tcl_FSEvalFile: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_FSNewNativePath tcl_FSNewNativePath: (@convention(c) (UnsafeMutablePointer<Tcl_Filesystem>!, ClientData!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_FSGetNativePath tcl_FSGetNativePath: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafePointer<Int8>!)!, tcl_FSFileSystemInfo tcl_FSFileSystemInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_FSPathSeparator tcl_FSPathSeparator: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_FSListVolumes tcl_FSListVolumes: (@convention(c) () -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_FSRegister tcl_FSRegister: (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Filesystem>!) -> Int32)!, tcl_FSUnregister tcl_FSUnregister: (@convention(c) (UnsafeMutablePointer<Tcl_Filesystem>!) -> Int32)!, tcl_FSData tcl_FSData: (@convention(c) (UnsafeMutablePointer<Tcl_Filesystem>!) -> ClientData!)!, tcl_FSGetTranslatedStringPath tcl_FSGetTranslatedStringPath: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> UnsafePointer<Int8>!)!, tcl_FSGetFileSystemForPath tcl_FSGetFileSystemForPath: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> UnsafeMutablePointer<Tcl_Filesystem>!)!, tcl_FSGetPathType tcl_FSGetPathType: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Tcl_PathType)!, tcl_OutputBuffered tcl_OutputBuffered: (@convention(c) (Tcl_Channel!) -> Int32)!, tcl_FSMountsChanged tcl_FSMountsChanged: (@convention(c) (UnsafeMutablePointer<Tcl_Filesystem>!) -> Void)!, tcl_EvalTokensStandard tcl_EvalTokensStandard: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Token>!, Int32) -> Int32)!, tcl_GetTime tcl_GetTime: (@convention(c) (UnsafeMutablePointer<Tcl_Time>!) -> Void)!, tcl_CreateObjTrace tcl_CreateObjTrace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!, Int32, UnsafePointer<Int8>!, Tcl_Command!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, ClientData!, (@convention(c) (ClientData!) -> Void)!) -> Tcl_Trace!)!, tcl_GetCommandInfoFromToken tcl_GetCommandInfoFromToken: (@convention(c) (Tcl_Command!, UnsafeMutablePointer<Tcl_CmdInfo>!) -> Int32)!, tcl_SetCommandInfoFromToken tcl_SetCommandInfoFromToken: (@convention(c) (Tcl_Command!, UnsafePointer<Tcl_CmdInfo>!) -> Int32)!, tcl_DbNewWideIntObj tcl_DbNewWideIntObj: (@convention(c) (Tcl_WideInt, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_GetWideIntFromObj tcl_GetWideIntFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_WideInt>!) -> Int32)!, tcl_NewWideIntObj tcl_NewWideIntObj: (@convention(c) (Tcl_WideInt) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_SetWideIntObj tcl_SetWideIntObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, Tcl_WideInt) -> Void)!, tcl_AllocStatBuf tcl_AllocStatBuf: (@convention(c) () -> UnsafeMutablePointer<Tcl_StatBuf>!)!, tcl_Seek tcl_Seek: (@convention(c) (Tcl_Channel!, Tcl_WideInt, Int32) -> Tcl_WideInt)!, tcl_Tell tcl_Tell: (@convention(c) (Tcl_Channel!) -> Tcl_WideInt)!, tcl_ChannelWideSeekProc tcl_ChannelWideSeekProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Tcl_WideInt, Int32, UnsafeMutablePointer<Int32>!) -> Tcl_WideInt)!)!, tcl_DictObjPut tcl_DictObjPut: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_DictObjGet tcl_DictObjGet: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, tcl_DictObjRemove tcl_DictObjRemove: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_DictObjSize tcl_DictObjSize: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_DictObjFirst tcl_DictObjFirst: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_DictSearch>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_DictObjNext tcl_DictObjNext: (@convention(c) (UnsafeMutablePointer<Tcl_DictSearch>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafeMutablePointer<Int32>!) -> Void)!, tcl_DictObjDone tcl_DictObjDone: (@convention(c) (UnsafeMutablePointer<Tcl_DictSearch>!) -> Void)!, tcl_DictObjPutKeyList tcl_DictObjPutKeyList: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_DictObjRemoveKeyList tcl_DictObjRemoveKeyList: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, tcl_NewDictObj tcl_NewDictObj: (@convention(c) () -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_DbNewDictObj tcl_DbNewDictObj: (@convention(c) (UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_RegisterConfig tcl_RegisterConfig: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_Config>!, UnsafePointer<Int8>!) -> Void)!, tcl_CreateNamespace tcl_CreateNamespace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, ClientData!, (@convention(c) (ClientData!) -> Void)!) -> UnsafeMutablePointer<Tcl_Namespace>!)!, tcl_DeleteNamespace tcl_DeleteNamespace: (@convention(c) (UnsafeMutablePointer<Tcl_Namespace>!) -> Void)!, tcl_AppendExportList tcl_AppendExportList: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Namespace>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_Export tcl_Export: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Namespace>!, UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_Import tcl_Import: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Namespace>!, UnsafePointer<Int8>!, Int32) -> Int32)!, tcl_ForgetImport tcl_ForgetImport: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Namespace>!, UnsafePointer<Int8>!) -> Int32)!, tcl_GetCurrentNamespace tcl_GetCurrentNamespace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Namespace>!)!, tcl_GetGlobalNamespace tcl_GetGlobalNamespace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> UnsafeMutablePointer<Tcl_Namespace>!)!, tcl_FindNamespace tcl_FindNamespace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_Namespace>!, Int32) -> UnsafeMutablePointer<Tcl_Namespace>!)!, tcl_FindCommand tcl_FindCommand: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_Namespace>!, Int32) -> Tcl_Command!)!, tcl_GetCommandFromObj tcl_GetCommandFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Tcl_Command!)!, tcl_GetCommandFullName tcl_GetCommandFullName: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!, tcl_FSEvalFileEx tcl_FSEvalFileEx: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!) -> Int32)!, tcl_SetExitProc tcl_SetExitProc: (@convention(c) ((@convention(c) (ClientData!) -> Void)!) -> (@convention(c) (ClientData!) -> Void)!)!, tcl_LimitAddHandler tcl_LimitAddHandler: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, ClientData!, (@convention(c) (ClientData!) -> Void)!) -> Void)!, tcl_LimitRemoveHandler tcl_LimitRemoveHandler: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, (@convention(c) (ClientData!, UnsafeMutablePointer<Tcl_Interp>!) -> Void)!, ClientData!) -> Void)!, tcl_LimitReady tcl_LimitReady: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!, tcl_LimitCheck tcl_LimitCheck: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!, tcl_LimitExceeded tcl_LimitExceeded: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!, tcl_LimitSetCommands tcl_LimitSetCommands: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Void)!, tcl_LimitSetTime tcl_LimitSetTime: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Time>!) -> Void)!, tcl_LimitSetGranularity tcl_LimitSetGranularity: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32, Int32) -> Void)!, tcl_LimitTypeEnabled tcl_LimitTypeEnabled: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!, tcl_LimitTypeExceeded tcl_LimitTypeExceeded: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!, tcl_LimitTypeSet tcl_LimitTypeSet: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Void)!, tcl_LimitTypeReset tcl_LimitTypeReset: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Void)!, tcl_LimitGetCommands tcl_LimitGetCommands: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!) -> Int32)!, tcl_LimitGetTime tcl_LimitGetTime: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Time>!) -> Void)!, tcl_LimitGetGranularity tcl_LimitGetGranularity: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Int32)!, tcl_SaveInterpState tcl_SaveInterpState: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> Tcl_InterpState!)!, tcl_RestoreInterpState tcl_RestoreInterpState: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_InterpState!) -> Int32)!, tcl_DiscardInterpState tcl_DiscardInterpState: (@convention(c) (Tcl_InterpState!) -> Void)!, tcl_SetReturnOptions tcl_SetReturnOptions: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_GetReturnOptions tcl_GetReturnOptions: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_IsEnsemble tcl_IsEnsemble: (@convention(c) (Tcl_Command!) -> Int32)!, tcl_CreateEnsemble tcl_CreateEnsemble: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, UnsafeMutablePointer<Tcl_Namespace>!, Int32) -> Tcl_Command!)!, tcl_FindEnsemble tcl_FindEnsemble: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, Int32) -> Tcl_Command!)!, tcl_SetEnsembleSubcommandList tcl_SetEnsembleSubcommandList: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_SetEnsembleMappingDict tcl_SetEnsembleMappingDict: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_SetEnsembleUnknownHandler tcl_SetEnsembleUnknownHandler: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_SetEnsembleFlags tcl_SetEnsembleFlags: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, Int32) -> Int32)!, tcl_GetEnsembleSubcommandList tcl_GetEnsembleSubcommandList: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, tcl_GetEnsembleMappingDict tcl_GetEnsembleMappingDict: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, tcl_GetEnsembleUnknownHandler tcl_GetEnsembleUnknownHandler: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, tcl_GetEnsembleFlags tcl_GetEnsembleFlags: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<Int32>!) -> Int32)!, tcl_GetEnsembleNamespace tcl_GetEnsembleNamespace: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Tcl_Command!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Namespace>?>!) -> Int32)!, tcl_SetTimeProc tcl_SetTimeProc: (@convention(c) ((@convention(c) (UnsafeMutablePointer<Tcl_Time>!, ClientData!) -> Void)!, (@convention(c) (UnsafeMutablePointer<Tcl_Time>!, ClientData!) -> Void)!, ClientData!) -> Void)!, tcl_QueryTimeProc tcl_QueryTimeProc: (@convention(c) (UnsafeMutablePointer<(@convention(c) (UnsafeMutablePointer<Tcl_Time>!, ClientData!) -> Void)?>!, UnsafeMutablePointer<(@convention(c) (UnsafeMutablePointer<Tcl_Time>!, ClientData!) -> Void)?>!, UnsafeMutablePointer<ClientData?>!) -> Void)!, tcl_ChannelThreadActionProc tcl_ChannelThreadActionProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Int32) -> Void)!)!, tcl_NewBignumObj tcl_NewBignumObj: (@convention(c) (UnsafeMutablePointer<mp_int>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_DbNewBignumObj tcl_DbNewBignumObj: (@convention(c) (UnsafeMutablePointer<mp_int>!, UnsafePointer<Int8>!, Int32) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_SetBignumObj tcl_SetBignumObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<mp_int>!) -> Void)!, tcl_GetBignumFromObj tcl_GetBignumFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<mp_int>!) -> Int32)!, tcl_TakeBignumFromObj tcl_TakeBignumFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<mp_int>!) -> Int32)!, tcl_TruncateChannel tcl_TruncateChannel: (@convention(c) (Tcl_Channel!, Tcl_WideInt) -> Int32)!, tcl_ChannelTruncateProc tcl_ChannelTruncateProc: (@convention(c) (UnsafePointer<Tcl_ChannelType>!) -> (@convention(c) (ClientData!, Tcl_WideInt) -> Int32)!)!, tcl_SetChannelErrorInterp tcl_SetChannelErrorInterp: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!, tcl_GetChannelErrorInterp tcl_GetChannelErrorInterp: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Void)!, tcl_SetChannelError tcl_SetChannelError: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!, tcl_GetChannelError tcl_GetChannelError: (@convention(c) (Tcl_Channel!, UnsafeMutablePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Void)!, tcl_InitBignumFromDouble tcl_InitBignumFromDouble: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, Double, UnsafeMutablePointer<mp_int>!) -> Int32)!, tcl_GetNamespaceUnknownHandler tcl_GetNamespaceUnknownHandler: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Namespace>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_SetNamespaceUnknownHandler tcl_SetNamespaceUnknownHandler: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Namespace>!, UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_GetEncodingFromObj tcl_GetEncodingFromObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafeMutablePointer<Tcl_Encoding?>!) -> Int32)!, tcl_GetEncodingSearchPath tcl_GetEncodingSearchPath: (@convention(c) () -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_SetEncodingSearchPath tcl_SetEncodingSearchPath: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!) -> Int32)!, tcl_GetEncodingNameFromEnvironment tcl_GetEncodingNameFromEnvironment: (@convention(c) (UnsafeMutablePointer<Tcl_DString>!) -> UnsafePointer<Int8>!)!, tcl_PkgRequireProc tcl_PkgRequireProc: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!, UnsafeMutablePointer<ClientData?>!) -> Int32)!, tcl_AppendObjToErrorInfo tcl_AppendObjToErrorInfo: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!) -> Void)!, tcl_AppendLimitedToObj tcl_AppendLimitedToObj: (@convention(c) (UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32, Int32, UnsafePointer<Int8>!) -> Void)!, tcl_Format tcl_Format: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafePointer<Int8>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> UnsafeMutablePointer<Tcl_Obj>!)!, tcl_AppendFormatToObj tcl_AppendFormatToObj: (@convention(c) (UnsafeMutablePointer<Tcl_Interp>!, UnsafeMutablePointer<Tcl_Obj>!, UnsafePointer<Int8>!, Int32, UnsafePointer<UnsafeMutablePointer<Tcl_Obj>?>!) -> Int32)!, tcl_ObjPrintf tcl_ObjPrintf: OpaquePointer!, tcl_AppendPrintfToObj tcl_AppendPrintfToObj: OpaquePointer!)
}
var tclStubsPtr: UnsafeMutablePointer<TclStubs>!
