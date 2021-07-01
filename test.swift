
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
        if ISREMOTE() {
            self.transport.RESIGN(address: self.address);
        }
    }
}

func ISREMOTE() -> Bool { 
    false
}
