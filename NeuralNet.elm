module NeuralNet exposing (..)

import Genetics exposing (Chromosome)
import Array exposing (Array)
import Matrix exposing (Matrix)
import Matrix.Random as Random
import Matrix.Extra as Matrix exposing (transpose, dotVector, dotColumn)
import Random exposing (Generator)
import Random.Floatx exposing (standardNormal)

-- forward-fed neural network represented by sequence of matrices
type alias NeuralNet = List (Matrix Float)

map : (Float -> Float) -> NeuralNet -> NeuralNet
map = List.map << Matrix.map

map2 : (Float -> Float -> Float) -> NeuralNet -> NeuralNet -> NeuralNet
map2 = List.map2 << Matrix.map2

map3 : (Float -> Float -> Float -> Float)
  -> NeuralNet -> NeuralNet -> NeuralNet -> NeuralNet
map3 = List.map3 << Matrix.map3


-- a generator for a forward-fed neural net
-- given list of number of neurons per layer
random : List Int -> Generator NeuralNet
random layers = let
  genConst n = Random.int n n
  genNormalMat n m = Random.matrix (genConst n) (genConst m) standardNormal
  genEmpty = Random.list 0 (genNormalMat 0 0)
 in case layers of
  [] -> genEmpty
  numInputs :: rest ->
    List.foldr (\(n, m) gen ->
      Random.map2 (::) (genNormalMat (n+1) m) gen
    ) genEmpty (List.map2 (,) layers rest)

propagate : NeuralNet -> Array Float -> Array Float
propagate net inputs = List.foldl
  (\mat vec -> dotColumn mat (Array.push 1 vec)) inputs net

crossover : Float -> NeuralNet -> NeuralNet -> NeuralNet
crossover u parent1 parent2 =
 let -- flatten parent matrices into lists of rows and crossover those lists
  numRows1 = List.map Array.length parent1
  rows1 = List.concatMap Array.toList parent1
  rows2 = List.concatMap Array.toList parent2
  rowsC = Genetics.crossover u rows1 rows2
 in -- reconstitute into a list of matrices based on original row count
  List.foldl (\n (child, rest) ->
    (Array.fromList (List.take n rest) :: child, List.drop n rest)
  ) ([], rowsC) numRows1 |> fst

mutate : Float -> NeuralNet -> NeuralNet -> NeuralNet -> (NeuralNet, NeuralNet)
mutate u u's sigmas net = let
  n = List.foldl (\mat n -> Matrix.colCount mat * Matrix.rowCount mat + n) 0 net
  tau_u = u / sqrt (toFloat (n + n))
  tau' = 1 / sqrt (2 * sqrt (toFloat n))
  sigma's = map2 (\u' sigma -> e^(tau_u + tau' * u') * sigma) u's sigmas
  net' = map3 (\u' sigma' x -> sigma' * u' + x) u's sigma's net
 in
  (net', sigma's)



-- list of input weights, but with output bias first
type alias Neuron = Array Float

tanh : Float -> Float
tanh x = let e2x = e^(x+x) in (e2x - 1) / (e2x + 1)

activate : Neuron -> Array Float -> Float
activate weights inputs = dotVector weights (Array.push 1 inputs)
