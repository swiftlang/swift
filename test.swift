
struct ADDRESS {} 

protocol TRANSPORT_PROTOCOL {
    func RESIGN(address: ADDRESS) 
}

struct TRANSPORT: TRANSPORT_PROTOCOL { 
    func RESIGN(address: ADDRESS) {
    }
}

class ACTOR { 
    let address: ADDRESS
    let transport: TRANSPORT_PROTOCOL

    init() { 
        self.address = ADDRESS() 
        self.transport = TRANSPORT()
    }

    deinit { 
        self.transport.RESIGN(address: self.address);
    }
}
