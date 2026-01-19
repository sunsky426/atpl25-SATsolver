# SAT-solving using grovers algorithm

## Code organization
The source code for the quantum circuit evaluators can be found inside the `src` folder, which consists of two folders:

- `GenEval`, which contains the code for the general quantum circuit evaluator using tensor rank decomposition
- `SpecEval`, which contains the code for the specialized quantum circuit evaluator using tensor rank decomposition

Both of these modules contain code for converting boolean expressions into phase oracles using ANF, and for evaluating these oracles using grovers algorithm.

The code for running the evaluators and benchmarking can be found inside the `exe` folder. Tests for the project is inside the `tests` folder.

The benchmarking code for the [HQP](github.com/diku-dk/atpl25-pub) library can be found in the file `hqp-bench.hs`, which uses the haskell library criterion. However this was run separately inside of HQP playground and cannot be run as is in this project folder, but is there mainly for inspection.

## Running 

Grovers algorithm can be run on single solution oracles that use one control-Z gate, and some number of X-gates. This is done by using commands of the following form to only get the solution:
```
cabal run grover -- <spec|gen>-one <number of qubits> [qubits to negate]
```
For example the following would be an example to run the specialized evaluator for a 5-qubit oracle with the only solution being 01010:
```sh
cabal run grover -- spec-one 5 0,2,4
```
It is also possible to print out the full statevector using commands of the form:
```
cabal run grover -- <spec|gen>-sv <number of qubits> [qubits to negate]
```
As an example:
```sh
cabal run grover -- spec-sv 5 0,2,4
```

### Solving General Boolean Formulae

The `SATQ` executable showcases the SAT-solving capabilities of Grover's algorithm, by allowing an arbitrary boolean formula to be evaluated through a select pipeline defined in `exe/SAT.hs`.
The solver takes a single `String` argument:
```
cabal run SATQ "<bool-exp>"
```
Where `<bool-exp>` takes the form of alphanumerical atoms between binary operators `|, &, ^` and unary `~`. An example could be:
```
cabal run SATQ "(a ^ (a & b)) | ~b"
b -> False, a -> False
```
The output assignments are printed to console, with parse-sensitive ordering. Parenthesis are not necessary, but forces the right relations.

The pipeline does not yet work for all boolean expressions.

## Benchmarking
Benchmarking for the evaluators can be run using commands of the form that specifies the type of oracle and what evaluator to use:
```
cabal run grover -- bench-<simp|complex|overlap>-<gen|spec> <number of qubits>
```
Here is an example of benchmarking with a complex oracle up to 9 qubits using the specialized evaluator:
```sh
cabal run grover -- bench-complex-spec 9
```
One can also save the results in a csv file and html file:
```sh
cabal run grover -- bench-complex-spec 9 --csv spec-complex.csv --output spec-complex.html
```

## Testing
Run `cabal test`
