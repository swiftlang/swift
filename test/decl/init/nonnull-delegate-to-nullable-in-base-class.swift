// RUN: %target-swift-frontend -emit-sil -verify %s

class Animal {
    convenience init?(species: String) {
        self.init()
    }
}

class Dog: Animal {
    var butt = "dog \("butt")"

    convenience init(owner: String) {
        self.init(species: "Dog")!
    }


}

print(Dog(owner: "John Arbuckle").butt)

