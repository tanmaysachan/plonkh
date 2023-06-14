module TestCircuit where

import Circuit

testingGates :: Circuit
testingGates = [Gate Add (Input Public "a") (Input Public "b") (Input Private "c") 0, Gate Mult (Input Private "c") (Input Public "d") (Input Private "e") 0, Gate Mult (Input Private "d") (Input Public "c") (Input Private "a") 0]

doms = cosetA ++ cosetB ++ cosetC
wis = (lefts testingGates) ++ (rights testingGates) ++ (outs testingGates)
