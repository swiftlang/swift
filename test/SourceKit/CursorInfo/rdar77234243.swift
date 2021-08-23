// RUN: %sourcekitd-test -req=cursor -pos=3:9 -req-opts=retrieve_symbol_graph=1 %s -- %s
extension NonExistentSymbolName {
    var myType: String {
        ""
    }
}
