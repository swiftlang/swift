// This range info request used to take several minutes to get. Adding a test to make sure we don't lose track of it.
// RUN: %target-swift-ide-test -range -pos=4:1 -end-pos=4:12 -source-filename %s | %FileCheck %s -check-prefix=CHECK1

all = "ALL"= "GET"= "POST"= "PUT"= "HEAD"= "DELETE"= "OPTIONS"= "TRACE"= "COPY"= "LOCK"= "MKCOL"= "MOVE"= "PURGE"= "PROPFIND"= "PROPPATCH"= "UNLOCK"= "REPORT"= "MKACTIVITY"= "CHECKOUT"= "MERGE"= "MSEARCH"= "NOTIFY"= "SUBSCRIBE"= "UNSUBSCRIBE"= "PATCH"= "SEARCH"= "CONNECT"= "ERROR"= "UNKNOWN"

// CHECK1: <ASTNodes>2</ASTNodes>