// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t 

// REQUIRES: concurrency

// SPECIFIER: Begin completions
// SPECIFIER-DAG: Keyword/None:                       async; name=async
// SPECIFIER-DAG: Keyword[throws]/None:               throws; name=throws
// SPECIFIER: End completions

// SPECIFIER_WITHASYNC: Begin completions
// SPECIFIER_WITHASYNC-NOT: async
// SPECIFIER_WITHASYNC-DAG: Keyword[throws]/None:               throws; name=throws
// SPECIFIER_WITHASYNC-NOT: async
// SPECIFIER_WITHASYNC: End completions

// SPECIFIER_WITHTHROWS: Begin completions
// SPECIFIER_WITHTHROWS-NOT: async
// SPECIFIER_WITHTHROWS-DAG: Keyword/None:               async; name=async
// SPECIFIER_WITHTHROWS-NOT: async
// SPECIFIER_WITHTHROWS: End completions

// SPECIFIER_WITHASYNCTHROWS-NOT: Begin completions

func testSpecifier() #^SPECIFIER^#
func testSpecifierWithAsync() async #^SPECIFIER_WITHASYNC^#
func testSpecifierWithThrows() throws #^SPECIFIER_WITHTHROWS^#
func testSpecifierWithAsyncThorws() async throws #^SPECIFIER_WITHASYNCTHROWS^#

func testTypeSpecifier(_: () #^TYPE_SPECIFICER?check=SPECIFIER^#) {}
func testTypeSpecifierWithAsync(_: () async #^TYPE_SPECIFICER_WITHASYNC?check=SPECIFIER_WITHASYNC^#) {}
func testTypeSpecifierWithThrows(_: () throws #^TYPE_SPECIFICER_WITHTHROWS?check=SPECIFIER_WITHTHROWS^#) {}
func testTypeSpecifierWithAsyncThrows(_: () async throws #^TYPE_SPECIFICER_WITHASYNCTHROWS?check=SPECIFIER_WITHASYNCTHROWS^#) {}
func testTypeSpecifierWithArrow(_: () #^TYPE_SPECIFICER_WITHARROW?check=SPECIFIER^#) {}
func testTypeSpecifierWithAsyncArrow(_: () async #^TYPE_SPECIFICER_WITHASYNCARROW?check=SPECIFIER_WITHASYNC^# -> Void) {}
func testTypeSpecifierWithThrowsArrow(_: () throws #^TYPE_SPECIFICER_WITHTHROWSARROW?check=SPECIFIER_WITHTHROWS^# -> Void
func testTypeSpecifierWithAsyncThrowsArrow(_: () async throws #^TYPE_SPECIFICER_WITHASYNCTHROWSARROW?check=SPECIFIER_WITHASYNCTHROWS^# -> Void) {}

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
