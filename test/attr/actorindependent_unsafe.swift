// RUN: %target-swift-frontend -typecheck -verify %s -enable-experimental-concurrency

// REQUIRES: concurrency

//////////////////////////
/// Cases that only work because of @actorIndependent(unsafe)
//////////////////////////

//////////////////////////
// 1 -- basic unsafe methods / properties accessing var member without annotation
actor class Actor1 {
  var counter : Int = 42
  
  @actorIndependent(unsafe)
  func aMethod() {
    counter += 1
  }

  @actorIndependent(unsafe)
  var computedProp : Int { counter / 2 }
}

let a1 = Actor1()
let _ = a1.aMethod()
let _ = a1.computedProp == a1.computedProp


//////////////////////////
// 2 -- more unsafe methods / properties accessing var member without annotation
actor class WeatherActor1 {
  var tempCelsius : Double = 5.0
  
  var tempFahrenheit : Double { 
    get { (1.8 * tempCelsius) + 32.0 }
    set { tempCelsius = (newValue - 32.0) / 1.8 }
  }

  @actorIndependent(unsafe)
  var tempCelsiusUnsafe : Double {
    get { tempCelsius }
    set { tempCelsius = newValue }
  }

  @actorIndependent
  var tempCelsiusPretendSafe : Double {
    get { tempCelsiusUnsafe }
    set { tempCelsiusUnsafe = newValue }
  }

  @actorIndependent(unsafe)
  var tempCelsiusUnsafe2 : Double {
    get { tempCelsius }
    set { tempCelsius = newValue }
  }

  @actorIndependent(unsafe)
  func getTempFahrenheitUnsafe() -> Double {
    return tempFahrenheit
  }

  @actorIndependent(unsafe)
  func setTempFahrenheitUnsafe(_ newValue : Double) {
    tempFahrenheit = newValue
  }
}

let wa1 = WeatherActor1()
let _ = wa1.setTempFahrenheitUnsafe(wa1.getTempFahrenheitUnsafe())
wa1.tempCelsiusUnsafe = wa1.tempCelsiusUnsafe + 1
wa1.tempCelsiusPretendSafe = wa1.tempCelsiusUnsafe
wa1.tempCelsiusUnsafe2 = wa1.tempCelsiusUnsafe2 + 2


//////////////////////////
// 3 -- basic actorIndependent accessing actorIndependent(unsafe) member
actor class Actor2 {
  
  @actorIndependent(unsafe)
  var counter : Int = 42
  
  @actorIndependent
  func aMethod() {
    counter += 1
  }

  @actorIndependent
  var computedProp : Int { counter / 2 }
}

let a2 = Actor2()
let _ = a2.aMethod()
let _ = a2.computedProp


//////////////////////////
// 4 -- more actorIndependent accessing actorIndependent(unsafe) member
actor class WeatherActor2 {
  @actorIndependent(unsafe)
  var tempCelsius : Double = 5.0
  
  @actorIndependent
  var tempFahrenheit : Double { 
    get { (1.8 * tempCelsius) + 32.0 }
    set { tempCelsius = (newValue - 32.0) / 1.8 }
  }

  @actorIndependent
  var tempCelsiusUnsafe : Double {
    get { tempCelsius }
    set { tempCelsius = newValue }
  }

  @actorIndependent
  var tempCelsiusUnsafe2 : Double {
    get { tempCelsiusUnsafe }
    set { tempCelsiusUnsafe = newValue }
  }

  @actorIndependent
  func getTempFahrenheitUnsafe() -> Double {
    return tempFahrenheit
  }

  @actorIndependent
  func setTempFahrenheitUnsafe(_ newValue : Double) {
    tempFahrenheit = newValue
  }
}

let wa2 = WeatherActor2()
let _ = wa2.setTempFahrenheitUnsafe(wa2.getTempFahrenheitUnsafe())
wa2.tempCelsiusUnsafe = wa2.tempCelsiusUnsafe + 1
wa2.tempCelsiusUnsafe2 = wa2.tempCelsiusUnsafe2 + 2


//////////////////////////
// 5 -- even more actorIndependent accessing actorIndependent(unsafe) member
actor class WeatherActor3 {
  
  @actorIndependent(unsafe)
  var tempCelsius : Double = 5.0
  
  @actorIndependent(unsafe)
  var tempFahrenheit : Double { 
    get { (1.8 * tempCelsius) + 32.0 }
    set { tempCelsius = (newValue - 32.0) / 1.8 }
  }

  subscript(info : String) -> Double {
    get {
      switch info {
      case "f", "fahrenheit": 
        return tempFahrenheit

      case "c", "celsius": 
        return tempCelsius
        
      default:
        print("setter for unknown weather information: " + info)
        return 0.0
      }
    }
    set {
      switch info {
      case "f", "fahrenheit": 
        tempFahrenheit = newValue

      case "c", "celsius": 
        tempCelsius = newValue
        
      default:
        print("getter for unknown weather information: " + info)
      }
    }
  }
}

let wa3 = WeatherActor3()
wa3.tempFahrenheit += 3
wa3.tempCelsius -= 1


//////////////////////////
// 6 -- accesses to static members

actor class Actor3 {
  @actorIndependent(unsafe)
  static var pi : Double = 3.0

  @actorIndependent(unsafe)
  static var e : Double = 2.0

  // expected-error@+1{{'@actorIndependent' can not be applied to stored properties}}
  @actorIndependent static var pi_2 : Double = 9.0

  static var e_2 : Double = 4.0
}

let _ = Actor3.pi + Actor3.e


//////////////////////////
// 7 -- accesses to global vars

@actorIndependent(unsafe)
var time = 1.12

actor class Actor4 {
  var currentTime : Double {
    get { time }
    set { time = newValue }
  }
}
