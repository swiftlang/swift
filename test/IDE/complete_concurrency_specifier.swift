// RUN: %batch-code-completion

// REQUIRES: concurrency

// SPECIFIER-DAG: Keyword/None:                       async; name=async
// SPECIFIER-DAG: Keyword[throws]/None:               throws; name=throws

// SPECIFIER_WITHASYNC-NOT: async
// SPECIFIER_WITHASYNC-DAG: Keyword[throws]/None:               throws; name=throws
// SPECIFIER_WITHASYNC-NOT: async

// SPECIFIER_WITHTHROWS-NOT: async
// SPECIFIER_WITHTHROWS-DAG: Keyword/None:               async; name=async
// SPECIFIER_WITHTHROWS-NOT: async

// SPECIFIER_WITHASYNCTHROWS-NOT: Begin completions

func testSpecifier() #^SPECIFIER^#
func testSpecifierWithAsync() async #^SPECIFIER_WITHASYNC^#
func testSpecifierWithThrows() throws #^SPECIFIER_WITHTHROWS^#
func testSpecifierWithAsyncThorws() async throws #^SPECIFIER_WITHASYNCTHROWS^#

func testTypeSpecifier(_: () #^TYPE_SPECIFIER?check=SPECIFIER^#) {}
func testTypeSpecifierWithAsync(_: () async #^TYPE_SPECIFIER_WITHASYNC?check=SPECIFIER_WITHASYNC^#) {}
func testTypeSpecifierWithThrows(_: () throws #^TYPE_SPECIFIER_WITHTHROWS?check=SPECIFIER_WITHTHROWS^#) {}
func testTypeSpecifierWithAsyncThrows(_: () async throws #^TYPE_SPECIFIER_WITHASYNCTHROWS?check=SPECIFIER_WITHASYNCTHROWS^#) {}
func testTypeSpecifierWithArrow(_: () #^TYPE_SPECIFIER_WITHARROW?check=SPECIFIER^#) {}
func testTypeSpecifierWithAsyncArrow(_: () async #^TYPE_SPECIFIER_WITHASYNCARROW?check=SPECIFIER_WITHASYNC^# -> Void) {}
func testTypeSpecifierWithThrowsArrow(_: () throws #^TYPE_SPECIFIER_WITHTHROWSARROW?check=SPECIFIER_WITHTHROWS^# -> Void
func testTypeSpecifierWithAsyncThrowsArrow(_: () async throws #^TYPE_SPECIFIER_WITHASYNCTHROWSARROW?check=SPECIFIER_WITHASYNCTHROWS^# -> Void) {}

_ = { () #^CLOSURE?check=SPECIFIER^# in }
_ = { () async #^CLOSURE_WITHASYNC?check=SPECIFIER_WITHASYNC^# in }
_ = { () throws #^CLOSURE_WITHTHROWS?check=SPECIFIER_WITHTHROWS^# in }
_ = { () async throws #^CLOSURE_WITHAASYNCTHROWS?check=SPECIFIER_WITHASYNCTHROWS^# in }
_ = { () #^CLOSURE_WITHARROW?check=SPECIFIER^# -> Void in }
_ = { () async #^CLOSURE_WITHASYNCARROW?check=SPECIFIER_WITHASYNC^# -> Void in }
_ = { () throws #^CLOSURE_WITHTHROWSARROW?check=SPECIFIER_WITHTHROWS^# -> Void in }
_ = { () async throws #^CLOSURE_WITHASYNCTHROWSARROW?check=SPECIFIER_WITHASYNCTHROWS^# -> Void in }

_ = { arg #^CLOSURE2?check=SPECIFIER^# in }
_ = { arg async #^CLOSURE2_WITHASYNC?check=SPECIFIER_WITHASYNC^# in }
_ = { arg throws #^CLOSURE2_WITHTHROWS?check=SPECIFIER_WITHTHROWS^# in }
_ = { arg async throws #^CLOSURE2_WITHAASYNCTHROWS?check=SPECIFIER_WITHASYNCTHROWS^# in }
_ = { arg #^CLOSURE2_WITHARROW?check=SPECIFIER^# -> Void in }
_ = { arg async #^CLOSURE2_WITHASYNCARROW?check=SPECIFIER_WITHASYNC^# -> Void in }
_ = { arg throws #^CLOSURE2_WITHTHROWSARROW?check=SPECIFIER_WITHTHROWS^# -> Void in }
_ = { arg async throws #^CLOSURE2_WITHASYNCTHROWSARROW?check=SPECIFIER_WITHASYNCTHROWS^# -> Void in }
