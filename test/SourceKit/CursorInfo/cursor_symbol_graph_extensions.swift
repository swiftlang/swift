// RUN: %sourcekitd-test -req=cursor -pos=9:28 -req-opts=retrieve_symbol_graph=1 %s -- %s

extension ResourceRecordType {
    public var debugDescription: String {
        public struct HostRecord<IPType: IP> {
        }
        extension HostRecord: Hashable {
            public struct StartOfAuthorityRecord {
                public var a
            }
        }
    }
}
