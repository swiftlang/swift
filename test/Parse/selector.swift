// RUN: %swift %s -parse -verify

func murderInRoom(room:Int) {}
func murderInRoom(room:Int) withWeapon(weapon:Int) {}
func murderInRoom(room:Int) withWeapon(weapon:Int)
    framingSuspect(suspect:Int) {}

protocol Suspect {
    func murderInRoom(room:Int)
    func murderInRoom(room:Int) withWeapon(weapon:Int)
    func murderInRoom(room:Int) withWeapon(weapon:Int)
        framingSuspect(suspect:Int)
}
