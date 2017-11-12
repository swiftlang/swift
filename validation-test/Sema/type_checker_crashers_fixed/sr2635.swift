// RUN: %target-swift-frontend %s -typecheck

//
//  main.swift
//  TypeBasics
//
//  Created by David Scrève on 16/11/2014.
//  Copyright (c) 2014 David Scrève. All rights reserved.
//

let constante : String = "Hello World"


let constante2 = "Hello World"

let caractere : Character = Array(constante.characters)[0]

let caractere2 : Character = "A"

var variable : String


var chaine = "Bonjour le monde"


var nombre  : Int = 3

nombre=5

var valeur : Int32

valeur = Int32(nombre)



print("la valeur vaut : \(valeur)")


if (valeur > 3)
{
    print(">3")
}
else
{
    print("<=3")
}

switch(valeur)
{
case 0:
    print("0");
    
case 1,2:
    print("1");
    
case 10...100:
    print("Interval");
    
default:
    print("default");
}

